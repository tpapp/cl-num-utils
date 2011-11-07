;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite differentiation-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

(addtest (differentiation-tests)
  differentiation1
  (let ((f #'sin)
        (fp #'cos))
    (ensure-same (differentiate f 0d0) (funcall fp 0d0))
    (ensure-same (differentiate f 0.5d0) (funcall fp 0.5d0))))
