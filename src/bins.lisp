;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; bins -- generic interface
;;;
;;; BINS are univariate mappings to FIXNUMs, either based on exact correspondence
;;; (discrete bins) or location on the real line (continuous bins).  They are the
;;; (univariate) building blocks for histograms, used as cross products when
;;; necessary.

(defgeneric bin-index (bins value)
  (:documentation "Return the index (a FIXNUM) that corresponds to VALUE in BIN."))

(defgeneric bin-locations (bins start end)
  (:documentation "Return a vector of locations.  If the second value is T, BINS are
  discrete (- END START) values are returned, otherwise BINS are contiguous intervals
  on the real line, and the returned vector has (1+ (- END START)) elements which
  denote the breakpoints."))

(defgeneric bin-location (bins index)
  (:documentation "Return the value or interval that corresponds to the bin with
  INDEX."))

;;; evenly distributed bins

(defstruct (even-bins (:constructor even-bins (width &optional (offset 0))))
  "Evenly distributed bins.  Especially fast as binning requires simple arithmetic."
  (offset nil :type real :read-only t)
  (width nil :type real :read-only t))

(defmethod bin-index ((even-bins even-bins) value)
  (values (floor (- value (even-bins-offset even-bins))
                 (even-bins-width even-bins))))

(defmethod bin-locations ((even-bins even-bins) start end)
  (bind (((:structure even-bins- offset width) even-bins))
    (values
      (numseq (+ (* start width) offset) nil :length (- (1+ end) start) :by offset)
      t)))

(defmethod bin-location ((even-bins even-bins) index)
  (bind (((:structure even-bins- offset width) even-bins)
         (left (+ (* index width) offset)))
    (make-interval left (+ left width))))

(defun pretty-bins (width n &key (min-step (default-min-step width))
                         (bias *pretty-bias*) (five-bias *pretty-five-bias*)) 
  "Bins with a pretty step size, calculated using PRETTY-STEP (see its
documentation)."
  (even-bins (pretty-step width n :min-step min-step :bias bias
                          :five-bias five-bias)))

(defun sturges-bins (width length)
  "Bins with a pretty step size, n based on length of data, calculated
using Sturges's rule. "
  (pretty-bins width (1+ (ceiling (log length 2)))))

;;; integer bins

(defstruct (integer-bins (:constructor integer-bins))
  "Integer bins, for exact categorization.  All integers (fixnums) are mapped to
  themselves, other values raise an error.")

(defmethod bin-index ((integer-bins integer-bins) value)
  (check-type value fixnum)
  value)

(defmethod bin-locations ((integer-bins integer-bins) start end)
  (numseq start (1- end)))
 
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



;;; Frequencies -- generic interface
;;; 
;;; Data structures for counting the frequencies of multivariate indexes (always of
;;; the same rank).  Building block for a histogram, but does not have information on
;;; where the indexes came from.  Frequencies can only be added, not set or
;;; subtracted.  The total is available, but is not necessarily cached/saved.  Some
;;; combinations of subscripts may be invalid or unavailable, in that case an error
;;; is raised.
;;; 
;;; !! define condition
;;; !! write methods for using an array as frequencies

(defgeneric add-observation (frequencies frequency &rest subscripts)
  (:documentation "Add FREQENCY to the frequency table at SUBSCRIPTS."))

(defgeneric total-frequency (frequencies)
  (:documentation "The sum of all frequencies."))

(defgeneric frequency (frequencies &rest subscripts)
  (:documentation "Return frequency at SUBSCRIPTS."))

(defgeneric relative-frequency (frequencies &rest subscripts)
  (:documentation "")
  (:method (frequencies &rest subscripts)
    (/ (apply #'frequency frequencies subscripts)
       (total-frequency frequencies))))

(defgeneric subscript-limits (frequencies)
  (:documentation "Return limits for all dimensions, as (START . END).
  SUBSCRIPT-RANK and SUBSCRIPT-LIMIT are defined to fall back to using this
  function."))

(defgeneric subscript-limit (frequencies dimension)
  (:documentation "Return the index range along DIMENSION, as (START . END).")
  (:method (frequencies dimension)
    (elt (subscript-limits frequencies) dimension)))

(defgeneric subscript-rank (frequencies)
  (:documentation "Return the number of dimensions.")
  (:method (frequencies)
    (length (subscript-limits frequencies))))

;;; implementation using hash tables

(defclass hashed-frequencies ()
  ((table :accessor table :initarg :table
          :initform (make-hash-table :test #'equal)
          :documentation "Hash table, using subscripts as keys.")
   (total-frequency :accessor total-frequency :initarg :total-frequency
                    :initform 0)
   (limits :accessor limits :initarg :limits)))

(defun sort-frequencies% (table)
  "Return a list of (subscripts . frequency) elements, sorted by lexicographic
ordering of subscripts.  Not exported."
  (sort (iter
          (for (subscripts frequency) :in-hashtable table)
          (collect (cons subscripts frequency)))
        (lambda (a b)
          (iter
            (for a-elt :in a)
            (for b-elt :in b)
            (cond
              ((<= a-elt b-elt) (return t))
              ((>= a-elt b-elt) (return nil)))
            (finally
             ;; should never get here, keys are unique
             (error "something weird is going on: duplicate keys in table"))))
        :key #'car))

(defparameter *frequency-print-width* 40
  "The number of columns used for printing frequencies using text symbols.  Does not
  include the space used by labels etc.")

(defun print-univariate-frequencies% (stream sorted-frequencies subscript-to-label)
  "Print univaritate frequencies from TABLE to stream using *'s.  Subscripts are
converted to labels using SUBSCRIPT-TO-LABEL (should return a string).  Not
exported."
  (when sorted-frequencies
    (iter
      (for s-f :in sorted-frequencies)
      (bind ((((subscript) . frequency) s-f)
             (label (funcall subscript-to-label subscript)))
        (collect (cons label frequency) :into labels-and-frequencies)
        (maximize (length label) :into label-width)
        (maximize frequency :into maximum-frequency)
        (finally 
         (let ((step (max (pretty-step maximum-frequency *frequency-print-width*) 1)))
           (loop for (label . frequency) in labels-and-frequencies
                 do (format stream "~&~vA " label-width label)
                    (loop repeat (floor frequency step) do (princ #\* stream)))
           (format stream "~&(each * = frequency of ~A)" step)))))))

(defmethod print-object ((hashed-frequencies hashed-frequencies) stream)
  (print-unreadable-object (hashed-frequencies stream :type t)
    (bind (((:slots-r/o table total-frequency limits) hashed-frequencies)
           (sorted-frequencies (sort-frequencies% table)))
      (format stream "total=~A rank=~A" total-frequency (length limits))
      (if (= (length limits) 1)
          (print-univariate-frequencies% stream sorted-frequencies
                                         #'format-number)
          (loop for (subscripts . frequency) in sorted-frequencies do
            (format stream "~&~A=~A" subscripts frequency))))))

(defmethod initialize-instance :after ((hashed-frequencies hashed-frequencies) 
                                       &key rank &allow-other-keys)
  (setf (limits hashed-frequencies) (make-array rank :initial-element nil)))

(defmethod add-observation ((hashed-frequencies hashed-frequencies) frequency
                            &rest subscripts)
  (assert (plusp frequency))
  (bind (((:slots table total-frequency limits) hashed-frequencies)
         (rank (length limits)))
    (assert (= rank (length subscripts)))
    (incf total-frequency frequency)
    (loop for index below rank
          for subscript in subscripts
          do (check-type subscript fixnum)
             (aif (aref limits index)
                  (progn
                    (minf (car it) subscript)
                    (maxf (cdr it) (1+ subscript)))
                  (setf (aref limits index) (cons subscript (1+ subscript)))))
    (incf (gethash subscripts table 0) frequency)))

(defmethod frequency ((hashed-frequencies hashed-frequencies) &rest subscripts)
  (gethash subscripts (table hashed-frequencies) 0))

(defmethod subscript-limits ((hashed-frequencies hashed-frequencies))
  (coerce (limits hashed-frequencies) 'list))


;;; histogram -- generic interface
;;; 
;;; A histogram is extends the FREQUENCIES interface with information on how the
;;; indexes were mapped to bins.  This can be queried with HISTOGRAM-LOCATIONS and
;;; used for plotting.

(defgeneric histogram-locations (histogram)
  (:documentation "Return locations along each dimension, with START and END
  corresponding to subscript limits."))

(defclass hashed-histogram (hashed-frequencies)
  ((bins :accessor bins :initarg :bins))
  (:documentation "Multivariate histogram using a hash table."))

(defparameter *hashed-histogram-print-width* 40)

(defun format-location (location)
  "Return location, formatted as a string."
  (etypecase location
    (interval (format nil "[~A,~A]"
                      (format-number (interval-left location))
                      (format-number (interval-right location))))
    (real (format-number location))))

(defmethod print-object ((hashed-histogram hashed-histogram) stream)
  (bind (((:slots-r/o table total-frequency bins limits) hashed-histogram)
         ((bin &rest other-bins) bins))
    (print-unreadable-object (hashed-histogram stream :type t)
      (format stream "total=~A rank=~A" total-frequency (length limits))
      (if other-bins
          (format stream " ~A cells (not printed)" (hash-table-count table))
          (print-univariate-frequencies%
           stream (sort-frequencies% table)
           (lambda (subscript) (format-location (bin-location bin subscript))))))))

(defun make-hashed-histogram (&rest bins)
  (make-instance 'hashed-histogram :bins bins :rank (length bins)))

(defmethod histogram-locations ((histogram hashed-histogram))
  (with-slots (limits bins) histogram
    (mapcar #'(lambda (limit bin)
                (bin-locations bin (car limit) (cdr limit)))
            limits bins)))

(defmethod add-observation ((histogram hashed-histogram) frequency &rest coordinates)
  (bind (((:slots-r/o bins) histogram))
    (assert (= (length coordinates) (length bins)))
    (apply #'call-next-method histogram frequency (mapcar (lambda (bin coordinate)
                                                            (bin-index bin coordinate))
                                                          bins coordinates))))

(defun histogram-from-sequence (sequence bin)
  (aprog1 (make-hashed-histogram bin)
    (map nil (curry #'add-observation it 1) sequence)))

(defun histogram-from-matrix (matrix &rest bins)
  (bind ((histogram (apply #'make-hashed-histogram bins))
         ((nrow ncol) (array-dimensions matrix)))
    (assert (= (subscript-rank histogram) ncol))
    (loop for row :below nrow do
      (apply #'add-observation histogram 1 
            (coerce (displace-subarray matrix row) 'list)))
    histogram))

;; (defclass binned-data ()
;;   ((indexes :accessor indexes :initarg :indexes)))

;; (defmethod indexes ((vector vector))
;;   vector)

;; (defmethod as-array ((binned-data binned-data) &key copy?)
;;   (maybe-copy-array (indexes binned-data) copy?))

;; (defgeneric bin-limit (binned-data)
;;   (:documentation "Return an integer which larger than all indexes (but does not
;;   have to be the smallest of such values).")
;;   (:method ((vector vector))
;;     (1+ (reduce #'max vector)))
;;   (:method ((binned-data binned-data))
;;     (bin-limit (indexes binned-data))))

;; (defgeneric bin-origin (binned-data bin-index)
;;   (:documentation "Return information on the particular bin (what value/range is
;;   mapped to this bin) if available.")
;;   (:method ((vector vector) bin-index)
;;     bin-index)
;;   (:method ((binned-data binned-data) bin-index)
;;     bin-index))

;; (defgeneric bin-origins (binned-data)
;;   (:documentation "Bin origin for all bins.")
;;   (:method (binned-data)
;;     (iter
;;       (for bin-index :below (bin-limit binned-data))
;;       (collect (bin-origin binned-data bin-index) :result-type vector))))

;; ;;; continuous bins

;; (defclass continuous-binned-data (binned-data)
;;   ((breaks :accessor breaks :initarg :breaks))
;;   (:documentation "Used for binning real numbers."))

;; (defmethod bin-limit ((binned-data continuous-binned-data))
;;   (length (breaks binned-data)))

;; (defmethod bin-origin ((binned-data continuous-binned-data) bin-index)
;;   (bind (((:slots-r/o breaks) binned-data))
;;     (make-interval (aref breaks bin-index)
;;                    (let ((right-index (1+ bin-index)))
;;                      (when (< right-index (length breaks))
;;                        (aref breaks right-index))))))

;; (defun bin-using-breaks (vector breaks &key (below 0)
;;                          (above (- (length breaks) 2)) copy? skip-check?)
;;   (make-instance 'continuous-binned-data
;;                  :indexes (map 'simple-fixnum-vector
;;                                (irregular-bins breaks :below below :above above
;;                                                :copy? copy?
;;                                                :skip-check? skip-check?)
;;                                vector)
;;                  :breaks breaks))

;; (defun bin-using-quantiles (vector quantiles)
;;   "Bin VECTOR using its quantiles.  Quantiles has to contain 0 and 1.  Highest
;; element is put in the last bin."
;;   (assert (and (vector-satisfies? quantiles #'<)
;;                (= (aref quantiles 0) 0)
;;                (= (vector-last quantiles) 1)))
;;   (bin-using-breaks vector (sample-quantiles vector quantiles)
;;                     :skip-check? t))

;; ;;; discrete bins

;; (defclass discrete-binned-data (binned-data)
;;   ((keys :accessor keys :initarg :keys)))

;; (defmethod bin-limit ((binned-data discrete-binned-data))
;;   (length (keys binned-data)))

;; (defmethod bin-origin ((binned-data discrete-binned-data) bin-index)
;;   (aref (keys binned-data) bin-index))

;; (defmethod bin-origins ((binned-data discrete-binned-data))
;;   (keys binned-data))

;; (defun bin-discrete (vector &key (test #'eql))
;;   "Bin discrete data, using TEST.  The implementation uses a hash-table, and
;; TEST has to be acceptable to MAKE-HASH-TABLE."
;;   (let ((table (make-hash-table :test test)))
;;     (map nil (lambda (v)
;;                (setf (gethash v table) t))
;;          vector)
;;     (let ((keys (sort (coerce (hash-table-keys table) 'vector) #'<)))
;;       (iter
;;         (for key :in-vector keys :with-index key-index)
;;         (setf (gethash key table) key-index))
;;       (make-instance 'discrete-binned-datann==
;;                      :indexes (map 'vector (lambda (v) (gethash v table)) vector)
;;                      :keys keys))))
