;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defun random-vector (length element-type &optional (arg (coerce 1 element-type)))
  (aprog1 (make-array length :element-type element-type)
    (dotimes (index length)
      (setf (aref it index) (random arg)))))
