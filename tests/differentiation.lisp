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

(addtest (differentiation-tests)
  elasticity1
  (let+ ((alpha 2d0)
         ((&flet f (x) (expt x alpha)))
         (elas (elasticity #'f)))
    (ensure-same (funcall elas 2d0) alpha)
    (ensure-same (funcall elas 7d0) alpha)))
