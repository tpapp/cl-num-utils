;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; bins -- generic interface
;;;
;;; BINS are univariate mappings to FIXNUMs, either based on exact
;;; correspondence (discrete bins) or location on the real line (continuous
;;; bins).  They are the (univariate) building blocks for histograms, used as
;;; cross products when necessary.

(defgeneric bin-index (bins value)
  (:documentation
   "Return the index (a FIXNUM) that corresponds to VALUE in BIN."))

(defgeneric bin-location (bins index)
  (:documentation
   "Return the value or interval that corresponds to the bin with INDEX."))

;;; evenly distributed bins

(defstruct (even-bins (:constructor even-bins (width &optional (offset 0))))
  "Evenly distributed bins.  Especially fast as binning requires simple
  arithmetic."
  (offset nil :type real :read-only t)
  (width nil :type real :read-only t))

(defmethod bin-index ((even-bins even-bins) value)
  (values (floor (- value (even-bins-offset even-bins))
                 (even-bins-width even-bins))))

(defmethod bin-location ((even-bins even-bins) index)
  (let+ (((&structure even-bins- offset width) even-bins)
         (left (+ (* index width) offset)))
    (interval left (+ left width))))

(defun pretty-bins (width n &key (min-step (default-min-step width))
                         (bias *pretty-bias*) (five-bias *pretty-five-bias*)) 
  "Bins with a pretty step size, calculated using PRETTY-STEP (see its
  documentation)."
  (even-bins (pretty-step width n :min-step min-step :bias bias
                          :five-bias five-bias)))

;;; integer bins

(defstruct (integer-bins (:constructor integer-bins))
  "Integer bins, for exact categorization.  All integers (fixnums) are mapped
  to themselves, other values raise an error.")

(defmethod bin-index ((integer-bins integer-bins) value)
  (check-type value fixnum)
  value)

(defmethod bin-location ((integer-bins integer-bins) index)
  index)

;; ;;; irregular bins

;; (declaim (inline within-breaks? in-bin?% find-bin%))

;; (defun in-bin?% (value index breaks)
;;   "Return non-nil iff VALUE is in the bin corresponding to INDEX.  No
;; error checking, for internal use."
;;   (within? (aref breaks index)
;;            value
;;            (aref breaks (1+ index))))

;; (defun find-bin% (value breaks right &aux (left 0))
;;   "Find the bin index for value.  BREAKS should be strictly
;; increasing.  The invariants 0 <= LEFT < RIGHT < (LENGTH BREAKS)
;; and (WITHIN-BREAKS? VALUE (AREF BREAKS LEFT) (AREF BREAKS RIGHT))
;; are maintaned and expected to be satisfied when calling this function.  For
;; internal use."
;;   (loop
;;     (when (= (1+ left) right)
;;       (return left))
;;     (let ((middle (floor (+ left right) 2)))
;;       (if (< value (aref breaks middle))
;;           (setf right middle)
;;           (setf left middle)))))

;; (defun irregular-bins (breaks &key copy? skip-check?
;;                        (below nil below-p) (above nil above-p))
;;   "Return a binning function for irregular bins with BREAKS (right continuous).
;; If copy?, BREAKS will be copied, otherwise it may share structure.  BREAKS
;; should be strictly increasing, this is checked unless SKIP-CHECK?.  When BELOW
;; and/or ABOVE are given, value below the first or after the last bin are binned
;; accordingly, otherwise an error is signalled."
;;   (let* ((breaks (if copy?
;;                      (if (vectorp breaks)
;;                          (copy-seq breaks)
;;                          (coerce breaks 'vector))
;;                      breaks))
;;          (right (1- (length breaks)))
;;          (left-boundary (aref breaks 0))
;;          (right-boundary (aref breaks right)))
;;     (unless skip-check?
;;       (assert (vector-satisfies? breaks #'<)))
;;     (lambda (value)
;;       (cond
;;         ((< value left-boundary)
;;          (if below-p
;;              below
;;              (error "~A is below ~A, the first break."
;;                     value left-boundary)))
;;         ((<= right-boundary value)
;;          (if above-p
;;              above
;;              (error "~A is above ~A, the last break."
;;                     value right-boundary)))
;;         (t (find-bin% value breaks right))))))

;;; utility functions

(defun format-bin-location (location)
  "Return location, formatted as a string."
  (etypecase location
    (interval (format nil "[~A,~A]"
                      (format-number (interval-left location))
                      (format-number (interval-right location))))
    (real (format-number location))))

(defun binary-search (sorted i)
  "Binary search for a number I on a sequence (vector preferred) sorted in
strictly increasing order (not checked) returning the index.  When I is not
found, return NIL."
  (let* ((sorted (coerce sorted 'vector))
         (left 0)
         (right (1- (length sorted))))
    (assert (<= 0 right) () "Vector has no elements.")
    (unless (<= (aref sorted left) i (aref sorted right))
      (return-from binary-search nil))
    (do () ((> left right) nil)
      (let* ((middle (floor (+ left right) 2))
             (middle-value (aref sorted middle)))
        (cond
          ((= middle-value i)
           (return-from binary-search middle))
          ((< middle-value i)
           (setf left (1+ middle)))
          (t
           (setf right (1- middle))))))))
