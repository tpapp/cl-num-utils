;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:cl-num-utils-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
        #:clunit
        ;; cl-num-utils subpackages
        #:cl-num-utils.arithmetic
        #:cl-num-utils.chebyshev
        #:cl-num-utils.interval
        #:cl-num-utils.num=
        #:cl-num-utils.utilities
        #:cl-num-utils.statistics)
  (:shadowing-import-from #:cl-num-utils.statistics #:mean :variance #:median)
  (:export
   #:run))

(cl:in-package :cl-num-utils-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all tests in the test suite."
  (run-suite 'tests :use-debugger interactive?))
