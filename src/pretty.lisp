;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun real-epsilon (number)
  "Return the `machine epsilon' for the type of number (only the type
is used, not the value).  For complex numbers, return the epsilon of
the corresponding real type, for rationals, the epsilon is that of a
single float."
  (etypecase number
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)
    (rational single-float-epsilon)
    (complex (real-epsilon (realpart number)))))

(defparameter *default-min-step-correction* 100
  "Default multiplier for correcting the machine epsilon.")

(defun default-min-step (width)
  "Default minimum step."
  (* *default-min-step-correction* (real-epsilon width)))

(defparameter *pretty-bias* 0d0
  "Default bias for PRETTY-STEP.")

(defparameter *pretty-five-bias* 0.1d0
  "Default bias to 5's for PRETTY-STEP.")

(defun pretty-step (width n &key 
                    (min-step (default-min-step width))
                    (bias *pretty-bias*) (five-bias *pretty-five-bias*))
  "Return a `pretty' (meaning 1, 2, or 5*10^n) step size, and the
number of fractional digits as the second value.  The algorithm is the
following: WIDTH is divided to N intervals, and increased to MIN-STEP
if necessary.  The logarithm is taken, to which BIAS is added.  The
step size will be based on the fractional part.  FIVE-BIAS, also
interpreted on a log scale, favors 5 over 2 as the first digit.  When
BIAS is zero, STEP always divides WIDTH to at most N intervals."
  (let+ ((raw-step (max (/ width (1+ n)) min-step))
         ((&values exponent residual) (floor (+ (coerce (log raw-step 10)
                                                        'double-float)
                                                bias)))
         (correction (cond
                       ((<= residual (- #.(log 2d0 10) five-bias)) 2)
                       ((<= residual #.(log 5d0 10)) 5)
                       (t 10))))
    (values (* correction (expt 10 exponent))
            (max 0 (- (if (= correction 10) -1 0) exponent)))))

