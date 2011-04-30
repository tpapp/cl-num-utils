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
