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

;;; binned-data
;;; 
;;; Data structures for handling data that has been mapped to bins.  Convenience
;;; functions for recovering where the data have been from (if possible), and
;;; for the number of bins.  A vector of positive fixnums (not checked) can
;;; also act as binned-data.

(defclass binned-data ()
  ((indexes :accessor indexes :initarg :indexes)))

(defmethod indexes ((vector vector))
  vector)

(defgeneric bin-limit (binned-data)
  (:documentation "Return an integer which larger than all indexes (but does not
  have to be the smallest of such values).")
  (:method ((vector vector))
    (1+ (reduce #'max vector)))
  (:method ((binned-data binned-data))
    (bin-limit (indexes binned-data))))

(defgeneric bin-origin (binned-data bin-index)
  (:documentation "Return information on the particular bin (what value/range is
  mapped to this bin) if available.")
  (:method ((vector vector) bin-index)
    bin-index)
  (:method ((binned-data binned-data) bin-index)
    bin-index))

(defgeneric bin-origins (binned-data)
  (:documentation "Bin origin for all bins.")
  (:method (binned-data)
    (iter
      (for bin-index :below (bin-limit binned-data))
      (collect (bin-origin binned-data bin-index) :result-type vector))))

;;; continuous bins

(defclass continuous-binned-data (binned-data)
  ((breaks :accessor breaks :initarg :breaks))
  (:documentation "Used for binning real numbers."))

(defmethod bin-limit ((binned-data continuous-binned-data))
  (length (breaks binned-data)))

(defmethod bin-origin ((binned-data continuous-binned-data) bin-index)
  (bind (((:slots-r/o breaks) binned-data))
    (make-interval (aref breaks bin-index)
                   (let ((right-index (1+ bin-index)))
                     (when (< right-index (length breaks))
                       (aref breaks right-index))))))

(defun bin-using-breaks (vector breaks &key (below 0)
                         (above (- (length breaks) 2)) copy? skip-check?)
  (make-instance 'continuous-binned-data
                 :indexes (map 'simple-fixnum-vector
                               (irregular-bins breaks :below below :above above
                                               :copy? copy?
                                               :skip-check? skip-check?)
                               vector)
                 :breaks breaks))

(defun bin-using-quantiles (vector quantiles)
  "Bin VECTOR using its quantiles.  Quantiles has to contain 0 and 1.  Highest
element is put in the last bin."
  (assert (and (vector-satisfies? quantiles #'<)
               (= (aref quantiles 0) 0)
               (= (vector-last quantiles) 1)))
  (bin-using-breaks vector (sample-quantiles vector quantiles)
                    :skip-check? t))

;;; discrete bins

(defclass discrete-binned-data (binned-data)
  ((keys :accessor keys :initarg :keys)))

(defmethod bin-limit ((binned-data discrete-binned-data))
  (length (keys binned-data)))

(defmethod bin-origin ((binned-data discrete-binned-data) bin-index)
  (aref (keys binned-data) bin-index))

(defmethod bin-origins ((binned-data discrete-binned-data))
  (keys binned-data))

(defun bin-discrete (vector &key (test #'eql))
  "Bin discrete data, using TEST.  The implementation uses a hash-table, and
TEST has to be acceptable to MAKE-HASH-TABLE."
  (let ((table (make-hash-table :test test)))
    (map nil (lambda (v)
               (setf (gethash v table) t))
         vector)
    (let ((keys (sort (coerce (hash-table-keys table) 'vector) #'<)))
      (iter
        (for key :in-vector keys :with-index key-index)
        (setf (gethash key table) key-index))
      (make-instance 'discrete-binned-data
                     :indexes (map 'vector (lambda (v) (gethash v table)) vector)
                     :keys keys))))
