;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite arithmetic-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))
