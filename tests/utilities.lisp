;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite utilities-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

(addtest (utilities-tests)
  demean-test
  (ensure-same (demean #(0 1 2)) (values #(-1 0 1) 1)))
