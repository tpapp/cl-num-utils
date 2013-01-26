;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite rootfinding-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

(addtest (rootfinding-tests)
  (let ((*rootfinding-delta-relative* 1e-6)
        (*lift-equality-test* (==* 1d-2)))
    (ensure-same (root-bisection #'identity (interval -1 2)) 0)
    (ensure-same (root-bisection (lambda (x)
                                   (expt (- x 5) 3))
                                 (interval -1 10))
                 5)))
