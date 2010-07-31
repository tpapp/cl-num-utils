;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite statistics-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (statistics-tests)
  test-mean
  (ensure-same (mean (ia 5)) 2)
  (ensure-same (mean (ia 9)) 4))

(addtest (statistics-tests)
  test-sample-var
 (ensure-same (sample-var (ia 9)) 15/2)
 (ensure-same (sample-var (ia 20)) 35))

(addtest (statistics-tests)
  test-sample-cov
 (ensure-same (sample-cov (ia 9) (ia 9)) (sample-var (ia 9)))
 (ensure-same (sample-cov #(2 3 5) #(7 11 13)) 13/3))
