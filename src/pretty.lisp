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

(defun pretty (x &key (bias *pretty-bias*) (five-bias *pretty-five-bias*))
  "Return a rational that is close to x, and is a multiple of 1,2 or 5 times a
power of 10.  The logarithm is taken, to which BIAS is added.  The result will
be based on the fractional part.  FIVE-BIAS, also interpreted on a log scale,
favors 5 over 2 as the first digit.  When BIAS favors larger values."
  (let+ (((&values exponent residual)
          (floor (+ (coerce (log x 10) 'double-float) bias)))
         (correction (cond
                       ((<= residual (- #.(log 2d0 10) five-bias)) 2)
                       ((<= residual #.(log 5d0 10)) 5)
                       (t 10))))
    (values (* correction (expt 10 exponent))
            (max 0 (- (if (= correction 10) -1 0) exponent)))))

(defun pretty-step (width n &key
                    (min-step (default-min-step width))
                    (bias *pretty-bias*) (five-bias *pretty-five-bias*))
  "Return a `pretty' (meaning 1, 2, or 5*10^n) step size, and the number of
fractional digits as the second value.  Uses PRETTY, but enforces a minimum.
When BIAS is 0,, STEP always divides WIDTH to at most N intervals."
  (pretty (max (/ width (1+ n)) min-step) :bias bias :five-bias five-bias))
