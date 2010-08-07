;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite interval-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (interval-tests)
  test-interval
  (let ((a (make-interval 2 1)))
    (ensure-same (interval-width a) 1)
    (ensure-same (interval-diff a) -1)
    (ensure-same (interval-midpoint a 0.25) 1.75)
    (ensure (not (positive-interval? a)))
    (ensure (negative-interval? a))
    (ensure-same (flip-interval a) (make-interval 1 2))
    (ensure-same (interval-intersection a (make-interval 1.5 2.5))
                 (make-interval 1.5 2))
    (ensure-same (extend-interval a 0.5 1)
                 (make-interval 2.5 0))))

;;; !!! do tests for range, frac, spacer
