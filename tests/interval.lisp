;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite interval-tests (tests))

(deftest test-interval (interval-tests)
  (let ((a (interval 1 2)))
    (assert-equality #'num= 1 (interval-length a))
    (assert-equality #'num= 1.25 (interval-midpoint a 0.25))
    (assert-equality #'num= (interval 1.25 1.8) (shrink-interval a 0.25 0.2))
    (assert-true (in-interval? a 1.5))
    (assert-true (in-interval? a 1))
    (assert-true (in-interval? a 2))
    (assert-false (in-interval? a 0.9))
    (assert-false (in-interval? a 2.1))
    (assert-condition error (interval 2 1))))

(deftest test-interval-hull (interval-tests)
  (let ((a (interval 1 2)))
    (assert-equality #'num= nil (interval-hull nil))
    (assert-equality #'num= a (interval-hull a))
    (assert-equality #'num= a (interval-hull '(1 1.5 2)))
    (assert-equality #'num= a (interval-hull #(1 1.5 2)))
    (assert-equality #'num= a (interval-hull #2A((1) (1.5) (2))))
    (assert-equality #'num= (interval -1 3)
        (interval-hull (list (interval 0 2) -1 #(3) '(2.5))))
    (assert-condition error (interval-hull #C(1 2)))))

(deftest test-split-interval (interval-tests)
  (let ((a (interval 10 20)))
    (assert-equality #'num= (vector (interval 10 13) (interval 13 14) (interval 14 20))
        (split-interval a (list (spacer 1) (relative 0.1) (spacer 2))))
    (assert-equality #'num= (vector (interval 10 16) (interval 16 20))
        (split-interval a (list (spacer) 4)))
    (assert-condition error (split-interval a (list 9)))
    (assert-condition error (split-interval a (list 6 7 (spacer))))))

(deftest test-extendf-interval (interval-tests)
  (let+ ((counter -1)
         (a (make-array 2 :initial-contents (list nil (interval 1 2)))))
    (extendf-interval (aref a (incf counter)) 3)
    (extendf-interval (aref a (incf counter)) 3)
    (assert-equality #'num= (vector (interval 3 3) (interval 1 3)) a)
    (assert-equality #'num= 1 counter)))

(deftest test-grid-in (interval-tests)
  (let ((*lift-equality-test* #'array=))
    (assert-equality #'num= #(0.0 0.5 1.0) (grid-in (interval 0.0 1.0) 3))
    (assert-equality #'num= #(0 2 4) (grid-in (interval 0 4) 3))))

(deftest test-subintervals-in (interval-tests)
  (let ((expected (vector (interval 0 1 :open-left? nil :open-right? t)
                          (interval 1 2 :open-left? nil :open-right? t)
                          (interval 2 3 :open-left? nil :open-right? nil))))
    (assert-equality #'num= (subintervals-in (interval 0 3) 3)
      expected)))

(deftest test-plusminus-interval (interval-tests)
  (assert-equality #'num= (interval 0.5 1.5) (plusminus-interval 1 0.5)))
