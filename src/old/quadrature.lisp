;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;;; Richardson extrapolation

(defstruct (richardson-extrapolation
            (:constructor richardson-extrapolation
                (coefficient iterations
                 &aux (diagonal (make-array iterations
                                            :element-type 'double-float)))))
  "Given A(h)=A_0 + \sum_{k=1}^\infty a_k h^{kp}, calculate approximations for
A given A(h q^{-k}), where the latter can be incorporated using ADD with
consecutive values for k=1,...,max_iter, which returns the latest A(0) as the
first and the largest relative change, which can be used to test termination.

The algorithm uses Richardson extrapolation, the required coefficient is q^k."
  (coefficient nil :type double-float)
  (n 0 :type fixnum)
  (diagonal nil :type (array double-float (*))))

(defmethod add ((re richardson-extrapolation) h)
  (let+ (((&structure-r/o richardson-extrapolation- coefficient n diagonal) re)
         (largest-relative-change 0d0)
         (last (coerce h 'double-float)))
    (when (= n (length diagonal))
      (error 'reached-maximum-iterations :n n))
    (loop with product := coefficient
          for m from 0 below n
          do (let ((correction (/ (- last (aref diagonal m))
                                  (1- product))))
               (setf (aref diagonal m) last)
               (maxf largest-relative-change (/ (abs correction) (abs last)))
               (incf last correction)
               (multf product coefficient)))
    (setf (aref diagonal n) last)
    (incf (richardson-extrapolation-n re))
    (values last largest-relative-change)))

;;;; iterative quadrature: generic interface

(defstruct iterative-quadrature
  (f nil :type (function (double-float) double-float))
  (a nil :type double-float)
  (b nil :type double-float)
  (h nil :type double-float)
  (n 0 :type fixnum)
  (sum 0d0 :type double-float))

(defgeneric refine-quadrature (quadrature)
  (:documentation ""))

(defgeneric richardson-coefficient (quadrature)
  (:documentation ""))

;;; trapezoidal quadrature

(defstruct (trapezoidal-quadrature
            (:include iterative-quadrature)
            (:constructor trapezoidal-quadrature%)))

(defun trapezoidal-quadrature (f a b)
  (with-doubles (a b)
    (trapezoidal-quadrature% :f f :a a :b b :h (- b a))))

(defmethod refine-quadrature ((quadrature trapezoidal-quadrature))
  ;; (declare (optimize speed))
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
  (with-doubles (a b)
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
  "Internal function implementing Romberg quadrature.  Requires an iterative
quadrature instance, a relative EPSILON and MIN-ITER for the stopping
criterion, and the maximum number of iterations allowed."
  (loop with re = (richardson-extrapolation
                   (richardson-coefficient quadrature) max-iter)
        do (let+ ((q (refine-quadrature quadrature))
                  ((&values q-extrapolated change) (add re q))
                  (n (richardson-extrapolation-n re)))
             (when (and (<= min-iter n)
                        (<= change epsilon))
               (return-from romberg-quadrature% (values q-extrapolated n))))))

(defun romberg-quadrature (f a b &key (epsilon (sqrt double-float-epsilon))
                                      (min-iter 5)
                                      (max-iter 20)
                                      open?)
  "Romberg quadrature of F on the interval [A,B] (or (A,B), when OPEN?).  The
iteration stops if the relative change is below EPSILON, but only after
MIN-ITER refinements (to avoid spurious premature convergence).  An error
occurs when MAX-ITER iterations are reached without convergence."
  (romberg-quadrature% (if open?
                           (midpoint-quadrature f a b)
                           (trapezoidal-quadrature f a b))
                       epsilon min-iter max-iter))
