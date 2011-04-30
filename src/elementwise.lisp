;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric emap-dimensions (object)
  (:documentation "Return dimensions of OBJECT, in a format that is understood by
  EMAP-COMMON-DIMENSIONS.")
  (:method ((array array))
    (array-dimensions array))
  (:method ((sequence sequence))
    (list (length sequence)))
  (:method (object)
    nil))

(defun emap-common-dimensions (dimensions1 dimensions2)
  "Unify dimensions or signal an error."
  (cond
    ((and dimensions1 dimensions2)
     (assert (common-length (list dimensions1 dimensions2)) ()
             "Rank mismatch between dimensions ~A and ~A."
             dimensions1 dimensions2)
     (mapcar (lambda (d1 d2)
               (cond
                ((and d1 d2)
                 (assert (= d1 d2) ()
                         "Dimension mismatch between ~A and ~A.")
                 d1)
                 (d1 d1)
                 (t d2)))
             dimensions1 dimensions2))
    (dimensions1 dimensions1)
    (t dimensions2)))

(defgeneric emap-next (object)
  (:documentation "Return a closure that returns successive elements of OBJECT, in
  row-major order.")
  (:method ((array array))
    (let ((index 0))
      (lambda ()
        (prog1 (row-major-aref array index)
          (incf index)))))
  (:method ((list list))
    (lambda ()
      (prog1 (car list)
        (setf list (cdr list)))))
  (:method ((sequence sequence))
    (let ((index 0))
      (lambda ()
        (prog1 (nth index sequence)
          (incf index)))))
  (:method (object)
    (constantly object)))

(defun emap (element-type function &rest objects)
  "Map OBJECTS elementwise using FUNCTION.  If the result is an array, it has the
given ELEMENT-TYPE."
  (bind ((dimensions (reduce #'emap-common-dimensions objects :key #'emap-dimensions))
         (next-functions (mapcar #'emap-next objects))
         ((:flet next-result ())
          (apply function (mapcar #'funcall next-functions))))
    (if dimensions
        (aprog1 (make-array dimensions :element-type element-type)
          (dotimes (index (array-total-size it))
            (setf (row-major-aref it index) (next-result))))
        (next-result))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-emap-common-numeric-type (&optional 
                                             (real-float-types '(single-float double-float)))
    "Given REAL-FLOAT-TYPES in order of increasing precision (this is important),
keep those which are available as array element types, and define a lookup table and
a function for determining the narrowest common numeric type amoung floats, also
allowing for complex versions of these types.  If no such float type can be found in
the list, return T."
    (let* ((real-float-types (remove-if (complement #'array-element-type-available)
                                        real-float-types))
           (n (length real-float-types))
           (2n (* 2 n))
           (all-float-types (concatenate 'vector real-float-types 
                                         (mapcar (curry #'list 'complex)
                                                 real-float-types)))
           (matrix (make-array (list 2n 2n) :element-type 'fixnum)))
      ;; fill matrix
      (dotimes (a 2n)
        (dotimes (b 2n)
          (bind (((:values a-complex a-i) (floor a n))
                 ((:values b-complex b-i) (floor b n)))
            (setf (aref matrix a b) (+ (* (max a-complex b-complex) n)
                                       (max a-i b-i))))))
      ;; define function
      `(defun emap-common-numeric-type (type-a type-b)
         (bind (((:flet type-id (type))
                 (cond
                   ,@(loop for id :from 0
                           for float-type :across all-float-types
                           collect `((subtypep type ',float-type) ,id))
                   ((subtypep type 'integer) :integer)
                   (t nil)))
                (a-id (type-id type-a))
                (b-id (type-id type-b))
                (float-types (load-time-value ,all-float-types))
                (matrix (load-time-value ,matrix)))
           ;; !! should be extended to handle integers, integer & float combinations
           (if (and a-id b-id)
               (cond
                 ((and (eq a-id :integer) (eq b-id :integer))
                  (load-time-value (upgraded-array-element-type 'integer)))
                 ((eq a-id :integer) (aref float-types b-id))
                 ((eq b-id :integer) (aref float-types a-id))
                 (t (aref float-types (aref matrix a-id b-id))))
               t)))))
  (define-emap-common-numeric-type))

(defun emap-type-of (object)
  (typecase object
    (array (array-element-type object))
    (otherwise (type-of object))))

(defun as-array-or-scalar (object)
  "Prepare argument for emap.  Needed for the extremely crude implementation that is
used at the moment."
  (typecase object
    (standard-object (as-array object))
    (array object)
    (otherwise object)))

(defmacro define-elementwise-operation (function arglist docstring elementwise-function)
  "Define elementwise operation FUNCTION with ARGLIST (should be a flat list of
arguments, no optional, key, rest etc)."
  ;; !! implementation note: this is the place to optimize, not done at all at the
  ;; !! moment, 
  `(defgeneric ,function ,arglist
     (:documentation ,docstring)
     (:method ,arglist
       (let ,(loop :for argument :in arglist :collect
                   `(,argument (as-array-or-scalar ,argument)))
         (emap (reduce #'emap-common-numeric-type (list ,@arglist) :key #'emap-type-of)
               #',elementwise-function ,@arglist)))))

(defmacro define-elementwise-reducing-operation (function bivariate-function
                                                  elementwise-function
                                                  documentation-verb)
  (check-type documentation-verb string)
  (check-types (function bivariate-function elementwise-function) symbol)
  `(progn
     (define-elementwise-operation ,bivariate-function (a b)
       ,(format nil "~:(~A~) A and B elementwise." documentation-verb)
       ,elementwise-function)
     (defun ,function (&rest objects)
       ,(format nil "~:(~A~) objects elementwise." documentation-verb)
       (assert objects () "Need at least one object.")
       (reduce #',bivariate-function objects))))

(define-elementwise-reducing-operation e+ e2+ + "add")
(define-elementwise-reducing-operation e* e2* * "multiply")
(define-elementwise-reducing-operation e- e2- - "subtract")
(define-elementwise-reducing-operation e/ e2/ / "divide")

(define-elementwise-operation eexpt (base power) "Elementwise EXPT." expt)

(define-elementwise-operation eexp (arg) "Elementwise EXP." exp)

(define-elementwise-operation elog (arg) "Elementwise LOG." log)

(define-elementwise-operation esqrt (arg) "Elementwise SQRT." sqrt)

(defgeneric ereduce (function object &key key initial-value)
  (:documentation "Elementwise reduce, traversing in row-major order.")
  (:method (function (array array) &key key initial-value)
    (reduce function (flatten-array array) :key key :initial-value initial-value))
  (:method (function (sequence sequence) &key key initial-value)
    (reduce function sequence :key key :initial-value initial-value))
  (:method (function object &key key initial-value)
    (reduce function (as-array object :copy? nil) :key key :initial-value initial-value)))

(defmacro define-elementwise-reduction (name function &optional 
                                        (docstring 
                                         (format nil "Elementwise ~A." function)))
  `(defun ,name (object)
     ,docstring
     (ereduce #',function object)))

(define-elementwise-reduction emax max)
(define-elementwise-reduction emin min)
