;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:cl-num-utils-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
        #:clunit
        #:cl-slice
        ;; cl-num-utils subpackages (alphabetical order)
        #:cl-num-utils.arithmetic
        #:cl-num-utils.chebyshev
        #:cl-num-utils.elementwise
        #:cl-num-utils.interval
        #:cl-num-utils.matrix
        #:cl-num-utils.matrix-shorthand
        #:cl-num-utils.num=
        #:cl-num-utils.quadrature
        #:cl-num-utils.statistics
        #:cl-num-utils.rootfinding
        #:cl-num-utils.utilities)
  (:shadowing-import-from #:cl-num-utils.statistics #:mean :variance #:median)
  (:export
   #:run))

(cl:in-package :cl-num-utils-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all tests in the test suite."
  (run-suite 'tests :use-debugger interactive?))
