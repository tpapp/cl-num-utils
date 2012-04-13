;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite chebyshev-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

(defun maximum-on-grid (f interval &optional (n-grid 1000))
  (loop for index below n-grid
        maximizing (funcall f
                            (interval-midpoint interval
                                               (/ index (1- n-grid))))))

(defun approximation-error (f f-approx interval &optional (n-grid 1000))
  (maximum-on-grid (lambda (x)
                     (abs-diff (funcall f x) (funcall f-approx x)))
                   interval n-grid))

(defun test-chebyshev-approximate (f interval n-polynomials test-interval
                                   &rest rest)
  (let ((f-approx (apply #'chebyshev-approximate f interval n-polynomials rest)))
    (approximation-error f f-approx test-interval)))

(addtest (chebyshev-tests)
  chebyshev-open-inf
  (ensure (<= (test-chebyshev-approximate (lambda (x) (/ x (+ 4 x)))
                                          (interval 2 t) 15 (interval 2 102))
              1e-5))
  (ensure (<= (test-chebyshev-approximate (lambda (x) (exp (- x)))
                                          (interval 0 t) 15 (interval 0 10)
                                          :n-points 30)
              1e-4)))
