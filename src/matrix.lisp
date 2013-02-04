;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:cl-num-utils.matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-num-utils.elementwise
        #:cl-num-utils.num=
        #:cl-num-utils.print-matrix
        #:cl-num-utils.utilities
        #:let-plus)
  (:export
   #:diagonal-vector
   #:diagonal-matrix
   #:lower-triangular-matrix
   #:upper-triangular-matrix
   #:triangular-matrix
   #:hermitian-matrix))

(in-package #:cl-num-utils.matrix)

(defgeneric diagonal-vector (matrix)
  (:documentation "Return the diagonal elements of MATRIX as a vector.")
  (:method ((matrix array))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (n (min nrow ncol)))
      (aprog1 (aops:make-array-like matrix :dimensions n)
        (dotimes (index n)
          (setf (aref it index) (aref matrix index index))))))
  (:method (matrix)
    (diagonal-vector (aops:as-array matrix))))

(defgeneric (setf diagonal-vector) (vector matrix)
  (:documentation "Set the diagonal elements of MATRIX using VECTOR.")
  (:method ((vector vector) (matrix array))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (n (min nrow ncol)))
      (assert (length= vector n))
      (dotimes (index n)
        (setf (aref matrix index index) (aref vector index))))
    vector))

;;; utility functions
(defun valid-sparse-type? (type)
  "Check if TYPE is a valid type for sparse matrices.  Only supertypes and subtypes of NUMBER are allowed."
  (or (subtypep type 'number) (subtypep 'number type)))

(defun ensure-valid-elements (array rank &rest predicates)
  "Convert OBJECT to an array, check that it

1. has the required rank,

2. has a valid sparse element type, and

3. that it satisfies PREDICATES.

Return the array."
  (let* ((array (aops:as-array array))
         (type (array-element-type array)))
    (assert (valid-sparse-type? type) ()
            "Array has element-type ~A, which cannot be used for a numeric matrix.")
    (assert (= (array-rank array) rank))
    (loop for predicate in predicates
          do (assert (funcall predicate array)))
    array))

(defun zero-like (array)
  "Return 0 coerced to the element type of ARRAY.  It is assumed that the latter satisfies VALID-SPARSE-TYPE?."
  (coerce 0 (array-element-type array)))

;;; diagonal matrices
(defstruct diagonal-matrix
  "Diagonal matrix.  The elements in the diagonal are stored in a vector."
  (elements nil :type vector))

(defun diagonal-matrix (elements)
  (make-diagonal-matrix :elements (ensure-valid-elements elements 1)))

(define-structure-let+ (diagonal-matrix) elements)

(defmethod aops:as-array ((diagonal-matrix diagonal-matrix))
  (let+ (((&diagonal-matrix elements) diagonal-matrix)
         (n (length elements)))
    (aprog1 (aops:make-array-like elements
                                  :dimensions (list n n)
                                  :initial-element 0)
      (dotimes (index n)
        (setf (aref it index index) (aref elements index))))))

(defmethod aops:element-type ((diagonal-matrix diagonal-matrix))
  (array-element-type (diagonal-matrix-elements diagonal-matrix)))

(defmethod aops:dims ((diagonal-matrix diagonal-matrix))
  (let ((n (length (diagonal-matrix-elements diagonal-matrix))))
    (list n n)))

;;; wrapped matrices
(defstruct wrapped-matrix
  "A matrix that has some special structure (eg triangular, symmetric/hermitian).  ELEMENTS is always a matrix.  Not used directly, not exported."
  (elements nil :type (array * (* *)) :read-only t))

(defmethod aops:element-type ((wrapped-matrix wrapped-matrix))
  (array-element-type (wrapped-matrix-elements wrapped-matrix)))

(defmethod aops:dims ((wrapped-matrix wrapped-matrix))
  (array-dimensions (wrapped-matrix-elements wrapped-matrix)))

;;; triangular matrices
(declaim (inline above-diagonal? below-diagonal?))

(defun above-diagonal? (row col)
  "Test if element with indexes row and col is (strictly) above the diagonal."
  (< row col))

(defun below-diagonal? (row col)
  "Test if element with indexes row and col is (strictly) below the diagonal."
  (> row col))

(defmacro define-wrapped-matrix (type elements struct-docstring
                                 (masked-test masked-string)
                                 check-and-convert-elements
                                 regularize-elements)
  (let+ (((&with-gensyms matrix stream row col))
         (elements-accessor `(,(symbolicate type '#:-elements) ,matrix)))
    `(progn
       (defstruct (,type (:include wrapped-matrix))
         ,struct-docstring)
       (defun ,type (,elements)
         "Create a lower-triangular-matrix."
         (,(symbolicate '#:make- type) :elements ,check-and-convert-elements))
       (defmethod aops:as-array ((,matrix ,type))
         (let+ ((,elements ,elements-accessor))
           ,@(splice-awhen regularize-elements
               it)
           ,elements))
       (defmethod print-object ((,matrix ,type) ,stream)
         (print-unreadable-object (,matrix ,stream :type t)
           (print-matrix ,elements-accessor ,stream
                         :masked-fn (lambda (,row ,col)
                                      (when (,masked-test ,row ,col)
                                        ,masked-string))))))))

(define-wrapped-matrix lower-triangular-matrix elements
    "Lower triangular matrix.  ELEMENTS in the upper triangle are treated as zero."
    (above-diagonal? ".")
    (ensure-valid-elements elements 2)
    (let+ ((zero (zero-like elements))
           ((nrow ncol) (array-dimensions elements)))
      (dotimes (row nrow)
        (loop for col from (1+ row) below ncol
              do (setf (aref elements row col) zero)))))

(define-wrapped-matrix upper-triangular-matrix elements
    "Upper triangular matrix.  ELEMENTS in the lower triangle are treated as zero."
    (below-diagonal? ".")
    (ensure-valid-elements elements 2)
    (let+ ((zero (zero-like elements)))
      (dotimes (row (array-dimension elements 0))
        (loop for col from 0 below row
              do (setf (aref elements row col) zero)))))

(deftype triangular-matrix ()
  "Triangular matrix (either lower or upper)."
  '(or lower-triangular-matrix upper-triangular-matrix))

;;; Hermitian matrix

(define-wrapped-matrix hermitian-matrix elements
    "Hermitian/symmetric matrix, with elements stored in the _lower_ triangle.

Implements _both_ real symmetric and complex Hermitian matrices --- as technically, real symmetric matrices are also Hermitian.  Complex symmetric matrices are _not_ implemented as a special matrix type, as they don't have any special properties (eg real eigenvalues, etc)."
    (above-diagonal? "*")
    (ensure-valid-elements elements 2 #'aops:square-matrix?)
    (dotimes (row (array-dimension elements 0))
      (loop for col from 0 below row
            do (setf (aref elements row col)
                     (conjugate (aref elements col row))))))

(defmacro define-elementwise-with-constant
    (type
     &key (functions '(e2* e2/))
          (elements-accessor (symbolicate type '#:-elements)))
  "Define binary elementwise operations for FUNCTION for all subclasses of wrapped-elements."
  `(progn
     ,@(loop :for function :in functions
             :collect
                `(defmethod ,function ((a ,type) (b number))
                   (,type (,function (,elements-accessor a) b)))
             :collect
                `(defmethod ,function ((a number) (b ,type))
                   (,type (,function a (,elements-accessor b)))))))

(defmacro define-elementwise-same-class
    (type
     &key (functions '(e2+ e2- e2*))
          (elements-accessor (symbolicate type '#:-elements)))
  "Define binary elementwise operations for FUNCTION for two arguments of the same class."
  `(progn
     ,@(loop for function in functions collect
                `(defmethod ,function ((a ,type) (b ,type))
                   (,type (,function (,elements-accessor a)
                                     (,elements-accessor b)))))))

(defmacro define-elementwise-as-array (type
                                       &key (functions '(e2+ e2- e2*)))
  "Define binary elementwise operations for FUNCTION, implemented by converting them to arrays."
  `(progn
     ,@(loop for function in functions
             collect `(defmethod ,function ((a ,type) b)
                        (,function (aops:as-array a) b))
             collect `(defmethod ,function (a (b ,type))
                        (,function a (aops:as-array b))))))

(defmacro define-elementwise-univariate
    (type &key (functions '(e1- e1/ eexp e1log esqrt))
               (elements-accessor (symbolicate type '#:-elements)))
  "Define unary elementwise operations for FUNCTION for all subclasses of wrapped-elements."
  `(progn
     ,@(loop :for function :in functions
             :collect
             `(defmethod ,function ((a ,type))
                (,type (,function (,elements-accessor a)))))))

(define-elementwise-as-array wrapped-matrix)

(define-elementwise-with-constant lower-triangular-matrix)
(define-elementwise-with-constant upper-triangular-matrix)
(define-elementwise-with-constant hermitian-matrix)
(define-elementwise-with-constant diagonal-matrix)

(define-elementwise-same-class lower-triangular-matrix)
(define-elementwise-same-class upper-triangular-matrix)
(define-elementwise-same-class hermitian-matrix)
(define-elementwise-same-class diagonal-matrix)

(define-elementwise-univariate lower-triangular-matrix)
(define-elementwise-univariate upper-triangular-matrix)
(define-elementwise-univariate hermitian-matrix)
(define-elementwise-univariate diagonal-matrix)

(defmethod num= ((a wrapped-matrix) (b wrapped-matrix)
               &optional (tolerance *num=-tolerance*))
    (and (equal (type-of a) (type-of b))
         (num= (aops:as-array a) (aops:as-array b) tolerance)))

(defmethod num= ((a diagonal-matrix) (b diagonal-matrix)
               &optional (tolerance *num=-tolerance*))
    (num= (diagonal-matrix-elements a) (diagonal-matrix-elements b) tolerance))


;;; transpose

(defmethod aops:transpose ((matrix lower-triangular-matrix))
  (upper-triangular-matrix
   (aops:transpose (lower-triangular-matrix-elements matrix))))

(defmethod aops:transpose ((matrix upper-triangular-matrix))
  (lower-triangular-matrix
   (aops:transpose (upper-triangular-matrix-elements matrix))))

(defmethod aops:transpose ((matrix hermitian-matrix))
  (if (subtypep (aops:element-type matrix) 'real)
      matrix
      (hermitian-matrix (aops:transpose (aops:as-array matrix)))))

(defmethod aops:transpose ((diagonal diagonal-matrix))
  diagonal)
