;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-num-utils.matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:diagonal-vector
   #:diagonal-matrix))

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

(defun check-array-valid-sparse-type (array)
  "Check that ARRAY can be used for constructing sparse matrices."
  (let ((type (array-element-type array)))
    (assert (valid-sparse-type? type) ()
            "Array has element-type ~A, which cannot be used for a numeric matrix.")))

;;; diagonal matrices
(defstruct diagonal-matrix
  "Diagonal matrix.  The elements in the diagonal are stored in a vector."
  (elements nil :type vector))

(defun diagonal-matrix (elements)
  (check-array-valid-sparse-type elements)
  (make-diagonal-matrix :elements elements))

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
