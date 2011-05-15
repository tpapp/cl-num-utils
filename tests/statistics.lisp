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

(addtest (statistics-tests)
  test-array-mean
  (bind ((v1 (ia 6))
         (v2 (e+ v1 3))
         (v3 (e+ v1 5))
         (vectors (vector v1 v2 v3))
         (vm (e+ v1 8/3))
         ((:flet v->a (v))
          (displace-array v '(2 3)))
         (am (v->a vm))
         (*lift-equality-test* #'==))
    (ensure-same (mean vectors) vm)
    (ensure-same (mean (map 'vector #'v->a vectors)) am)
    (ensure-error (mean (list v1 am)))))

(defun naive-weighted-variance (sample weights)
  "Calculate weighted variance (and mean, returned as the second value) using
the naive method."
  (let* ((sw (sum weights))
         (mean (/ (reduce #'+ (map 'vector #'* sample weights)) sw))
         (variance (/ (reduce #'+
                              (map 'vector 
                                   (lambda (s w) (* (expt (- s mean) 2) w))
                                   sample weights))
                      (1- sw))))
    (values variance mean)))

(addtest (statistics-tests)
  test-weighted
  (let ((s1 #(1 2 3))
        (w1 #(4 5 6))
        (*lift-equality-test* #'==)
        (s2 (random-vector 50 'double-float))
        (w2 (random-vector 50 'double-float)))
    (ensure-same (naive-weighted-variance s1 w1) (weighted-variance s1 w1))
    (ensure-same (naive-weighted-variance s2 w2) (weighted-variance s2 w2)
                 :test (lambda (x y)
                         (< (/ (abs (- x y))
                               (max 1 (abs x) (abs y)))
                            1d-5)))
    (ensure-same (second (multiple-value-list (weighted-variance s1 w1)))
                 (weighted-mean s1 w1))))

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
