;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite chebyshev-tests (tests))

(defun maximum-on-grid (f interval &optional (n-grid 1000))
  "Maximum of F on a grid of N-GRID equidistand points in INTERVAL."
  (loop for index below n-grid
        maximizing (funcall f
                            (interval-midpoint interval
                                               (/ index (1- n-grid))))))

(defun approximation-error (f f-approx interval &optional (n-grid 1000))
  "Approximation error, using MAXIMUM-ON-GRID."
  (maximum-on-grid (lambda (x)
                     (abs-diff (funcall f x) (funcall f-approx x)))
                   interval n-grid))

(defun test-chebyshev-approximate (f interval n-polynomials test-interval
                                   &rest rest)
  (let ((f-approx (apply #'chebyshev-approximate f interval n-polynomials rest)))
    (approximation-error f f-approx test-interval)))

(deftest chebyshev-open-inf (chebyshev-tests)
  (assert-true (<= (test-chebyshev-approximate (lambda (x) (/ x (+ 4 x)))
                                               (interval 2 (xreal:inf)) 15
                                               (interval 2 102))
                   1e-5))
  (assert-true (<= (test-chebyshev-approximate (lambda (x) (exp (- x)))
                                               (interval 0 (xreal:inf)) 15
                                               (interval 0 10)
                                               :n-points 30)
                   1e-4)))

(deftest chebyshev-finite-interval (chebyshev-tests)
  (assert-true (<= (test-chebyshev-approximate (lambda (x) (/ (1+ (expt x 2))))
                                               (interval -3d0 2d0) 20
                                               (interval -1.5d0 1d0))
                   1e-3)))
