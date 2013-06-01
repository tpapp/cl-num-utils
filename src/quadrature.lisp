;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-num-utils.quadrature
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-num-utils.arithmetic
        #:cl-num-utils.interval
        #:cl-num-utils.utilities
        #:let-plus)
  (:export
   #:romberg-quadrature))

(cl:in-package #:cl-num-utils.quadrature)

;;;; Richardson extrapolation (general framework)

(defstruct (richardson-extrapolation
            (:constructor richardson-extrapolation
                (coefficient iterations
                 &aux (diagonal (make-array iterations
                                            :element-type 'double-float)))))
  "Given A(h)=A_0 + \sum_{k=1}^\infty a_k h^{kp}, calculate approximations for A given A(h q^{-k}), where the latter can be incorporated using RICHARDSON-ITERATION with consecutive values for k=1,...,max_iter, which returns the latest A(0) as the first and the largest relative change, which can be used to test termination.

The algorithm uses Richardson extrapolation, the required coefficient is q^k."
  (coefficient nil :type double-float)
  (n 0 :type fixnum)
  (diagonal nil :type (array double-float (*))))

(defun richardson-iteration (extrapolation step)
  "Add STEP (= $A(h q^{-k}$) to an existing Richardson EXTRAPOLATION.  See the documentation of RICHARDSON-EXTRAPOLATION for details."
  (let+ (((&structure-r/o richardson-extrapolation- coefficient n diagonal)
          extrapolation)
         (largest-relative-change 0d0)
         (step (coerce step 'double-float)))
    (when (= n (length diagonal))
      (error 'reached-maximum-iterations :n n))
    (loop with product := coefficient
          for m from 0 below n
          do (let ((correction (/ (- step (aref diagonal m))
                                  (1- product))))
               (setf (aref diagonal m) step)
               (maxf largest-relative-change (/ (abs correction) (abs step)))
               (incf step correction)
               (multf product coefficient)))
    (setf (aref diagonal n) step)
    (incf (richardson-extrapolation-n extrapolation))
    (values step largest-relative-change)))

;;;; iterative quadrature: generic interface

(defstruct iterative-quadrature
  "Quadrature building block.

F is the function.

A and B are the endpoints.

H is the stepsize."
  (f nil :type (function (double-float) double-float))
  (a nil :type double-float)
  (b nil :type double-float)
  (h nil :type double-float)
  (n 0 :type fixnum)
  (sum 0d0 :type double-float))

(defgeneric refine-quadrature (quadrature)
  (:documentation "Refine quadrature with more points.  Return the sum for those points."))

(defgeneric richardson-coefficient (quadrature)
  (:documentation "Return the coefficient $q$ for Richardson approximation."))

;;; trapezoidal quadrature

(defstruct (trapezoidal-quadrature
            (:include iterative-quadrature)
            (:constructor trapezoidal-quadrature%)))

(defun trapezoidal-quadrature (f a b)
  (with-double-floats (a b)
    (trapezoidal-quadrature% :f f :a a :b b :h (- b a))))

(defmethod refine-quadrature ((quadrature trapezoidal-quadrature))
  (let+ (((&structure-r/o iterative-quadrature- a b f) quadrature)
         ((&structure iterative-quadrature- n h sum) quadrature))
    (setf sum
          (if (zerop n)
              (* (+ (funcall f a) (funcall f b)) h 0.5d0)
              (+ (/ sum 2)
                 (let* ((h h))
                   (* h
                      (loop
                        repeat (expt 2 (1- n))
                        for x from (+ a h) by (* 2 h)
                        summing (funcall f x)))))))
    (incf n)
    (multf h 1/2)
    sum))

(defmethod richardson-coefficient ((quadrature trapezoidal-quadrature))
  4d0)

;;; midpoint quadrature

(defstruct (midpoint-quadrature
            (:include iterative-quadrature)
            (:constructor midpoint-quadrature%)))

(defun midpoint-quadrature (f a b)
  (with-double-floats (a b)
    (midpoint-quadrature% :f f :a a :b b :h (- b a))))

(defmethod refine-quadrature ((quadrature midpoint-quadrature))
  ;; (declare (optimize speed))
  (let+ (((&structure-r/o iterative-quadrature- a b f) quadrature)
         ((&structure iterative-quadrature- n h sum) quadrature))
    (setf sum
          (if (zerop n)
              (* h (+ (funcall f (/ (+ a b) 2))))
              (+ (/ sum 3)
                 (let* ((h h)
                        (2h (* 2 h))
                        (s 0d0))
                   (loop
                     repeat (expt 3 (1- n))
                     with x = (+ a (/ h 2))
                     do (incf s (funcall f x))
                        (incf x 2h)
                        (incf s (funcall f x))
                        (incf x h))
                   (* h s)))))
    (incf n)
    (multf h 1/3)
    sum))

(defmethod richardson-coefficient ((quadrature midpoint-quadrature))
  9d0)

;;; implementation of Romberg quadrature

(defun romberg-quadrature% (quadrature epsilon min-iter max-iter)
  "Internal function implementing Romberg quadrature.  Requires an iterative quadrature instance, a relative EPSILON and MIN-ITER for the stopping criterion, and the maximum number of iterations allowed.  Works on finite intervals."
  (loop with re = (richardson-extrapolation
                   (richardson-coefficient quadrature) max-iter)
        do (let+ ((q (refine-quadrature quadrature))
                  ((&values q-extrapolated change) (richardson-iteration re q))
                  (n (richardson-extrapolation-n re)))
             (when (and (<= min-iter n)
                        (<= change epsilon))
               (return-from romberg-quadrature% (values q-extrapolated n))))))

(defgeneric transformed-quadrature (function interval transformation)
  (:documentation "Return a quadrature for integrating FUNCTION on INTERVAL, which may be infinite, in which case FUNCTION will be transformed.  TRANSFORMATION can be used to select the transformation when applicable, otherwise it is NIL.")
  (:method (function (interval finite-interval) (transformation null))
    (let+ (((&interval (a a-open?) (b b-open?)) interval))
      (if (or a-open? b-open?)
          (midpoint-quadrature function a b)
          (trapezoidal-quadrature function a b))))
  (:method (function (interval plusinf-interval) (transformation null))
    (let+ (((&accessors-r/o left) interval))
      (midpoint-quadrature (lambda (y)
                             (let ((1-y (- 1 y)))
                               (/ (funcall function (+ left (/ y 1-y)))
                                  (expt 1-y 2))))
                           0 1))))

(defun romberg-quadrature (f interval &key (epsilon (sqrt double-float-epsilon))
                                           (min-iter 5)
                                           (max-iter 20)
                                           transformation)
  "Romberg quadrature of F on the interval.  The iteration stops if the relative change is below EPSILON, but only after MIN-ITER refinements (to avoid spurious premature convergence).  An error occurs when MAX-ITER iterations are reached without convergence."
  (romberg-quadrature% (transformed-quadrature f interval transformation)
                       epsilon min-iter max-iter))
