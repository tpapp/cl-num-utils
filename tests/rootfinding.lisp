;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:in-package #:cl-num-utils-tests)

(defsuite rootfinding-tests (tests))

(deftest bisection-test (rootfinding-tests)
  (let ((*rootfinding-delta-relative* 1e-6)
        (*num=-tolerance* 1d-2))
    (assert-equality #'num= 0 (root-bisection #'identity (interval -1 2)))
    (assert-equality #'num= 5
        (root-bisection (lambda (x)
                          (expt (- x 5) 3))
                        (interval -1 10)))))
