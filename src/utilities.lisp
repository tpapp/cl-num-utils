;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun demean (object &optional (mean (mean object)))
  "Subtract mean from object.  The mean is returned as the second value."
  (values (e- object mean) mean))
