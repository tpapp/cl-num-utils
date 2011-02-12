;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun even-bins (&key (offset 0) (width 1))
  "Evenly distributed bins of WIDTH, starting at OFFSET."
  (lambda (value)
    (floor (+ offset value) width)))

(defun pretty-bins (width n &key (min-step (default-min-step width))
                    (bias *pretty-bias*) (five-bias *pretty-five-bias*)) 
  "Bins with a pretty step size, calculated using PRETTY-STEP (see its
documentation)."
  (even-bins :width (pretty-step width n :min-step min-step :bias bias
                                 :five-bias five-bias)))
(defun integer-bins ()
  "Bin that round to the nearest integer."
  (even-bins :offset 0.5))

(defun sturges-bins (width length)
  "Bins with a pretty step size, n based on length of data, calculated
using Sturges's rule. "
  (pretty-bins width (1+ (ceiling (log length 2)))))

;;; irregular bins

(declaim (inline within-breaks? in-bin?% find-bin%))

(defun within? (left value right)
  "Return non-nil iff value is in [left,right)."
  (and (<= left value) (< value right)))

(defun in-bin?% (value index breaks)
  "Return non-nil iff VALUE is in the bin corresponding to INDEX.  No
error checking, for internal use."
  (within? (aref breaks index)
           value
           (aref breaks (1+ index))))

(defun find-bin% (value breaks right &aux (left 0))
  "Find the bin index for value.  BREAKS should be strictly
increasing.  The invariants 0 <= LEFT < RIGHT < (LENGTH BREAKS)
and (WITHIN-BREAKS? VALUE (AREF BREAKS LEFT) (AREF BREAKS RIGHT))
are maintaned and expected to be satisfied when calling this function.  For
internal use."
  (loop
    (when (= (1+ left) right)
      (return left))
    (let ((middle (floor (+ left right) 2)))
      (if (< value (aref breaks middle))
          (setf right middle)
          (setf left middle)))))

(defun irregular-bins (breaks &key copy? skip-check?
                       (below nil below-p) (above nil above-p))
  "Return a binning function for irregular bins with BREAKS (right continuous).
If copy?, BREAKS will be copied, otherwise it may share structure.  BREAKS
should be strictly increasing, this is checked unless SKIP-CHECK?.  When BELOW
and/or ABOVE are given, value below the first or after the last bin are binned
accordingly, otherwise an error is signalled."
  (let* ((breaks (if copy?
                     (if (vectorp breaks)
                         (copy-seq breaks)
                         (coerce breaks 'vector))
                     breaks))
         (right (1- (length breaks)))
         (left-boundary (aref breaks 0))
         (right-boundary (aref breaks right)))
    (unless skip-check?
      (assert (vector-satisfies? breaks #'<)))
    (lambda (value)
      (cond
        ((< value left-boundary)
         (if below-p
             below
             (error "~A is below ~A, the first break."
                    value left-boundary)))
        ((<= right-boundary value)
         (if above-p
             above
             (error "~A is above ~A, the last break."
                    value right-boundary)))
        (t (find-bin% value breaks right))))))
