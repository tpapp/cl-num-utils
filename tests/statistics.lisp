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
  test-variance
 (ensure-same (variance (ia 9)) 15/2)
 (ensure-same (variance (ia 20)) 35))

;; (addtest (statistics-tests)
;;   test-covariance
;;  (ensure-same (covariance (ia 9) (ia 9)) (variance (ia 9)))
;;  (ensure-same (covariance #(2 3 5) #(7 11 13)) 13/3))

(addtest (statistics-tests)
  quantiles
  (let ((sample #(0.0 1.0))
        (quantiles (numseq 0 1 :length 11 :type 'double-float)))
    (ensure-same (sample-quantiles sample quantiles)
                 quantiles)))
