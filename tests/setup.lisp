;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:cl-num-utils-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
        #:clunit)
  (:export
   #:run))

(cl:in-package :cl-num-utils-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all tests in the test suite."
  (run-suite 'tests :use-debugger interactive?))
