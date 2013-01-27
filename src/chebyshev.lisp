;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(defpackage #:cl-num-utils.chebyshev
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-num-utils.interval
        #:cl-num-utils.utilities
        #:let-plus)
  (:export
   #:chebyshev-root
   #:chebyshev-roots
   #:chebyshev-regression
   #:chebyshev-evaluate
   #:chebyshev-approximate))

(in-package #:cl-num-utils.chebyshev)

(declaim (inline chebyshev-recursion))
(defun chebyshev-recursion (x value previous-value)
  "Chebyshev polynomial recursion formula."
  (- (* 2 x value) previous-value))

(declaim (inline chebyshev-root))
(defun chebyshev-root (m i)
  "Return the iTH root of the Mth Chebyshev polynomial as double-float."
  (assert (within? 0 i m))
  (- (cos (/ (* (+ i 1/2) (float pi 1d0)) m))))

(defun chebyshev-roots (m)
  "Return the roots of the Mth Chebyshev polynomial as a vector of
double-floats."
  (aprog1 (make-array m :element-type 'double-float)
    (dotimes (i m)
      (setf (aref it i) (chebyshev-root m i)))))

(defun chebyshev-regression (f n-polynomials
                             &optional (n-points n-polynomials))
  "Chebyshev polynomial regression using the given number of polynomials and
points (zeroes of the corresponding Chebyshev polynomial)."
  (check-types (n-polynomials n-points) positive-fixnum)
  (assert (<= n-polynomials n-points) ()
          "Can't identify ~A coefficients with only ~A points."
          n-polynomials n-points)
  (locally (declare (optimize speed)
                    (type positive-fixnum n-polynomials n-points))
    (let+ ((z (the simple-double-float-vector (chebyshev-roots n-points)))
           (f-at-z (map 'simple-double-float-vector
                        (lambda (z) (coerce (funcall f z) 'double-float)) z))
           (coefficients (make-array n-points :element-type 'double-float))
           (values z)
           previous-values
           ((&flet weighted-sum (values)
              (/ (loop for v across values
                       for f across f-at-z
                       summing (* f v))
                 (/ n-points 2)))))
      (declare (type simple-double-float-vector
                     z f-at-z values previous-values))
      (loop for j from 0 below n-polynomials
            do (setf (aref coefficients j)
                     (if (zerop j)
                         (/ (reduce #'+ f-at-z) n-points)
                         (progn
                           (cond
                             ((= j 1) (weighted-sum z))
                             ((= j 2) (setf values
                                            (map 'simple-double-float-vector
                                                 (lambda (z)
                                                   (chebyshev-recursion z z 1d0))
                                                 z)))
                             ((= j 3)
                              (setf previous-values values
                                    values (map 'simple-double-float-vector
                                                #'chebyshev-recursion
                                                z previous-values z)))
                             (t (map-into previous-values
                                          #'chebyshev-recursion z values previous-values)
                                (rotatef values previous-values)))
                           (weighted-sum values)))))
      coefficients)))

(defun chebyshev-evaluate (coefficients x)
  "Return the sum of Chebyshev polynomials, weighted by COEFFICIENTS, at X."
  (let ((value (coerce x 'double-float))
        (previous-value 1d0)
        (sum 0d0))
    (dotimes (index (length coefficients))
      (incf sum (* (aref coefficients index)
                   (cond
                     ((= index 0) 1d0)
                     ((= index 1) x)
                     (t (setf previous-value (chebyshev-recursion x value previous-value))
                        (rotatef value previous-value)
                        value)))))
    sum))



(declaim (inline ab-to-cinf cinf-to-ab ab-to-cd-intercept-slope))

(defun cinf-to-ab (x a b c)
  "Map x in [c,plus-infinity) to z in [a,b] using x -> (x-c)/(1+x-c)+(b-a)+a."
  (let ((xc (- x c)))
    (assert (<= 0 xc) () "Value outside domain.")
    (+ (* (/ xc (1+ xc)) (- b a)) a)))

(defun ab-to-cinf (z a b c)
  "Inverse of cinf-to-ab."
  (let ((z-norm (/ (- z a) (- b a))))
    (assert (within? 0 z-norm 1) () "Value outside domain.")
    (+ c (/ z-norm (- 1 z-norm)))))

(defun ab-to-cd-intercept-slope (a b c d)
  "Return (values INTERCEPT SLOPE) for linear mapping x:-> intercept+slope*x
from [a,b] to [c,d]."
  (let ((b-a (- b a)))
    (values (/ (- (* b c) (* a d)) b-a)
            (/ (- d c) b-a))))

(defun chebyshev-approximate (f interval n-polynomials
                              &key (n-points n-polynomials))
  "Return a closure approximating F on the given INTERVAL (may be infinite on
either end) using the given number of Chebyshev polynomials."
  (chebyshev-approximate-implementation f interval n-polynomials n-points))

(defgeneric chebyshev-approximate-implementation (f interval n-polynomials
                                                  n-points)
  (:documentation "Implementation of CHEBYSHEV-APPROXIMATE.")
  (:method (f (interval plusinf-interval) n-polynomials n-points)
    (let+ (((&interval (left open-left?) &ign) interval)
           (a (if open-left?
                  -1d0
                  (chebyshev-root n-points 0)))
           (left (coerce left 'double-float))
           (coefficients
            (chebyshev-regression (lambda (z)
                                    (funcall f (ab-to-cinf z a 1d0 left)))
                                  n-polynomials n-points)))
      (lambda (x)
        (chebyshev-evaluate coefficients (cinf-to-ab x a 1d0 left)))))
  (:method (f (interval finite-interval) n-polynomials n-points)
    (let+ (((&interval (left open-left?) (right open-right?)) interval)
           ((&assert (< left right)))
           (a (if open-left?
                  -1d0
                  (chebyshev-root n-points 0)))
           (b (if open-right?
                  1d0
                  (chebyshev-root n-points (1- n-points))))
           ((&values intercept slope) (ab-to-cd-intercept-slope left right a b))
           (coefficients (chebyshev-regression (lambda (z)
                                                 (funcall f (/ (- z intercept)
                                                               slope)))
                                               n-polynomials n-points)))
      (lambda (x)
        (chebyshev-evaluate coefficients (+ intercept (* slope x)))))))
