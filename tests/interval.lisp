;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite interval-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

(addtest (interval-tests)
  test-interval
  (let ((a (interval 1 2))
        (*lift-equality-test* #'==))
    (ensure-same (interval-length a) 1)
    (ensure-same (interval-midpoint a 0.25) 1.25)
    (ensure-same (shrink-interval a 0.25 0.2)
                 (interval 1.25 1.8))
    (ensure (in-interval? a 1.5))
    (ensure (in-interval? a 1))
    (ensure (in-interval? a 2))
    (ensure (not (in-interval? a 0.9)))
    (ensure (not (in-interval? a 2.1)))
    (ensure-error (interval 2 1))))

(addtest (interval-tests)
  test-interval-hull
  (let ((a (interval 1 2)))
    (ensure-same (interval-hull nil) nil)
    (ensure-same (interval-hull a) a)
    (ensure-same (interval-hull '(1 1.5 2)) a)
    (ensure-same (interval-hull #(1 1.5 2)) a)
    (ensure-same (interval-hull #2A((1) (1.5) (2))) a)
    (ensure-same (interval-hull (list (interval 0 2) -1 #(3) '(2.5)))
                 (interval -1 3))
    (ensure-error (interval-hull #C(1 2)))))

(addtest (interval-tests)
  test-split-interval
  (let ((a (interval 10 20)))
    (ensure-same (split-interval a (list (spacer 1)
                                         (relative 0.1)
                                         (spacer 2)))
                 (vector (interval 10 13)
                         (interval 13 14)
                         (interval 14 20)))
    (ensure-same (split-interval a (list (spacer)
                                         4))
                 (vector (interval 10 16)
                         (interval 16 20)))
    (ensure-error (split-interval a (list 9)))
    (ensure-error (split-interval a (list 6 7 (spacer))))))

(addtest (interval-tests)
  test-extendf-interval
  (let+ ((counter -1)
         (a (make-array 2 :initial-contents (list nil (interval 1 2)))))
    (extendf-interval (aref a (incf counter)) 3)
    (extendf-interval (aref a (incf counter)) 3)
    (ensure-same a (vector (interval 3 3) (interval 1 3)))
    (ensure-same counter 1)))

(addtest (interval-tests)
  test-grid-in
  (let ((*lift-equality-test* #'array=))
    (ensure-same (grid-in (interval 0.0 1.0) 3)
                 (vector* (type-of 0.5) 0.0 0.5 1.0))
    (ensure-same (grid-in (interval 0 4) 3)
                 (vector* 'fixnum 0 2 4))))
