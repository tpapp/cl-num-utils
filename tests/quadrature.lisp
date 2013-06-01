;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite quadrature-tests (tests))

(deftest integration-finite (quadrature-tests)
  (flet ((test-romberg (function interval value &rest rest)
           (let+ (((&interval a b) interval)
                  (closed-interval (interval a b))
                  (open-interval (interval a b :open-left? t :open-right? t)))
             (assert-equality (num=-function 1e-5)
                 (apply #'romberg-quadrature function closed-interval rest)
                 value)
             (assert-equality (num=-function 1e-5)
                 (apply #'romberg-quadrature function open-interval rest)
                 value))))
    (test-romberg (constantly 1d0) (interval 0 2) 2d0)
    (test-romberg #'identity (interval 1 5) 12d0)
    (test-romberg (lambda (x) (/ (exp (- (/ (expt x 2) 2)))
                                 (sqrt (* 2 pi))))
                  (interval 0 1) 0.3413447460685429d0 :epsilon 1d-9)))

(deftest integration-plusinf (quadrature-tests)
  (assert-equality #'num= 1
      (romberg-quadrature (lambda (x) (expt x -2))
                          (interval 1 (xreal:inf))))
  (assert-equality #'num= 1/3
      (romberg-quadrature (lambda (x) (exp (* -3 x)))
                          (interval 0 (xreal:inf)))))
