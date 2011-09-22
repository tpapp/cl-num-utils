;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite interval-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (interval-tests)
  test-interval
  (let ((a (interval 2 1))
        (b (interval 1 2)))
    (ensure-same (interval-width a) 1)
    (ensure-same (interval-diff a) -1)
    (ensure-same (interval-midpoint a 0.25) 1.75)
    (ensure (not (positive-interval? a)))
    (ensure (negative-interval? a))
    (ensure-same (flip-interval a) b)
    (ensure-same (interval-intersection a (interval 1.5 2.5))
                 (interval 1.5 2))
    (ensure-same (shrink-interval b 0.25 0.2)
                 (interval 1.25 1.8))))

(addtest (interval-tests)
  test-limits
  (let ((a (interval 1 2)))
    (ensure-same (limits nil) nil)
    (ensure-same (limits a) a)
    (ensure-same (limits '(1 1.5 2)) a)
    (ensure-same (limits #(1 1.5 2)) a)
    (ensure-same (limits #2A((1) (1.5) (2))) a)
    (ensure-same (limits (list (interval 2 0) -1 #(3) '(2.5)))
                 (interval -1 3))
    (ensure-error (limits #C(1 2)))))

;;; !!! do tests for frac and spacer
