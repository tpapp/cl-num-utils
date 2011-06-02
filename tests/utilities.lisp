;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defun random-vector (length element-type &optional (arg (coerce 1 element-type)))
  (aprog1 (make-array length :element-type element-type)
    (dotimes (index length)
      (setf (aref it index) (random arg)))))

(defun array= (array1 array2)
  "Test that arrays are equal and have the same element type."
  (and (type= (array-element-type array1)
              (array-element-type array2))
       (equalp array1 array2)))

(defun array* (dimensions element-type &rest elements)
  "Return a (SIMPLE-ARRAY ELEMENT-TYPE dimensions) containing ELEMENTS,
coerced to ELEMENT-TYPE."
  (aprog1 (make-array dimensions :element-type element-type)
    (dotimes (index (array-total-size it))
      (assert elements () "Not enough elements.")
      (setf (row-major-aref it index) (coerce (car elements) element-type)
            elements (cdr elements)))
    (assert (not elements) () "Too many elements (~A)." elements)))

(defun vector* (element-type &rest elements)
  "Return a (SIMPLE-ARRAY ELEMENT-TYPE (*)) containing ELEMENTS,
coerced to ELEMENT-TYPE."
  (apply #'array* (length elements) element-type elements))

(defun iseq (n &optional (type 'fixnum))
  "Return a sequence of integers.  If type is LIST, a list is returned,
otherwise a vector with the corresponding upgraded element type."
  (if (eq type 'list)
      (loop for i below n collect i)
      (aprog1 (make-array n :element-type type)
        (dotimes (i n)
          (setf (aref it i) (coerce i type))))))
