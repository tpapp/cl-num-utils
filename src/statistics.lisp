;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Generic interface

(defgeneric add (accumulator object)
  (:documentation "Add accumulator to object."))

(defgeneric conforming-accumulator (statistic element)
  (:documentation "Return an accumulator that provides the desired
  STATISTIC and handles objects similar to ELEMENT."))

(defmacro with-accumulator ((accumulator add) &body body)
  "Provide a local wrapper for ADD which creates the conforming accumulator if
ACCUMULATOR is a symbol the first time it is used.  Once done, the form
evaluates to this accumulator.  For use in SWEEP."
  (once-only (accumulator)
    (with-unique-names (first?)
      `(let+ ((,first? t)
              ((&flet ,add (object)
                 (when (and ,first? (symbolp ,accumulator))
                   (setf ,accumulator
                         (conforming-accumulator ,accumulator object)))
                 (add ,accumulator object))))
         ,@body
         ,accumulator))))

(defgeneric sweep (accumulator object)
  (:documentation "Apply ACCUMULATOR to elements of OBJECT.  When ACCUMULATOR
  is a function, it is used to generate a conforming accumulator.")
  (:method (accumulator (sequence sequence))
    (with-accumulator (accumulator add)
      (map nil #'add sequence)))
  (:method (accumulator (array array))
    (with-accumulator (accumulator add)
      (map nil #'add (flatten-array array)))))

;;; sweep also resolves some symbols and functions into default accumulators,
;;; the helper macros below take care of defining these

(defmacro define-conforming-accumulator ((statistics object-specialized)
                                         &body body)
  "Specialize CONFORMING-ACCUMULATOR to given symbols and objects."
  `(progn
     ,@(loop for statistic in (ensure-list statistics) collect
             `(defmethod conforming-accumulator ((statistic (eql ',statistic))
                                                 ,object-specialized)
                ,@body))))

;;; When the object is not an accumulator, accessors fall back to sweeping it
;;; and then return the desired value.  In this case they may return the
;;; accumulator as the second value, which can be used for other queries.

(defgeneric tally (object)
  (:documentation "Return the count of elements.")
  (:method (object)
    (let ((accumulator (sweep 'tally object)))
      (values (tally accumulator) accumulator)))
  (:method ((sequence sequence))
    (length sequence))
  (:method ((array array))
    (array-total-size array)))

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method (object)
    (let ((accumulator (sweep 'mean object)))
      (values (mean accumulator) accumulator))))

(defgeneric sse (object &optional center)
  (:documentation "Return the sum of squared errors (from the given CENTER,
  which defaults to the MEAN.  Return NIL when there are no elements.")
  (:method (object &optional center)
    (let ((accumulator (sweep 'sse object)))
      (values (sse accumulator center) accumulator))))

(defgeneric variance (object)
  (:documentation "Return the variance.")
  (:method (object)
    (let+ (((&values sse accumulator) (sse object))
           (n-1 (1- (aif accumulator
                         (tally it)
                         (tally object)))))
      (when (plusp n-1)
        (/ sse n-1)))))

(defgeneric sum (object &key key)
  (:documentation "Sum of elements in object.  KEY is applied to each element.")
  ;; !!! TODO: also with accumulators
  (:method ((sequence sequence) &key (key #'identity))
    (reduce #'+ sequence :key key))
  (:method ((array array) &key (key #'identity))
    (reduce #'+ (flatten-array array) :key key)))

(defgeneric product (object)
  (:documentation "Product of elements in object.")
  ;; !!! TODO: also with accumulators
  (:method ((sequence sequence))
    (reduce #'* sequence))
  (:method ((array array))
    (reduce #'* (flatten-array array))))

(defgeneric quantile (object q)
  (:documentation "Return an element at quantile Q.  May be an interpolation
  or an approximation, depending on OBJECT and Q.")
  (:method (object q)
    (let ((accumulator (sweep 'quantiles object)))
      (values (quantile accumulator q) accumulator))))

;; (defun median (object)
;;   "Median of OBJECT."
;;   (quantile object 0.5))

;;; Specific accumulators

;;; tallier

(defstruct (tallier (:constructor tallier ()))
  (tally 0 :type fixnum))

(defmethod add ((tallier tallier) object)
  (incf (tallier-tally tallier)))

(define-structure-slot-accessor tally tallier :read-only? t)

(define-conforming-accumulator (tally object)
  (tallier))

;;; mean accumulator for scalars

(defstruct (mean-accumulator
             (:include tallier)
             (:constructor mean-accumulator ()))
  "Accumulator for a (scalar) mean."
  (mean 0d0))

(define-modify-macro incf-mean (value tally)
  (lambda (mean value tally) (+ mean (/ (- value mean) tally)))
  "When MEAN is the MEAN of (1- TALLY) numbers, update it with VALUE.  In
  practice, TALLY should be INCF'd before using incf-mean.")

(defmethod add ((instance mean-accumulator) (object number))
  (bind (((:structure/rw mean-accumulator- tally mean) instance))
    (incf tally)
    (incf-mean mean object tally)))

(define-structure-slot-accessor mean mean-accumulator :read-only? t)

(define-conforming-accumulator (mean (number number))
  (mean-accumulator))

;;; mean accumulator for arrays

(defstruct+ (array-mean-accumulator
             (:constructor array-mean-accumulator% (mean)))
    "Array of accumulators."
  (tally 0 :type fixnum)
  (mean nil :type array :read-only t))

(defun array-mean-accumulator (array)
  "Create an array mean accumulator for array."
  (array-mean-accumulator% (make-array (array-dimensions array)
                                       :initial-element 0d0)))

(defmethod add ((accumulator array-mean-accumulator) object)
  (let+ (((&array-mean-accumulator tally nil) accumulator)
         ((&array-mean-accumulator-r/o nil mean) accumulator)
         (array (aprog1 (as-array object)
                  (assert (common-dimensions it mean))))
         (tally (incf tally)))
    (dotimes (index (array-total-size array))
      (incf-mean (row-major-aref mean index)
                 (row-major-aref array index) tally))))

(define-structure-slot-accessor mean array-mean-accumulator :read-only? t)

(define-conforming-accumulator (mean (array array))
  (array-mean-accumulator array))

;;; mean-sse accumulator

(defstruct (mean-sse-accumulator
             (:constructor mean-sse-accumulator ())
             (:include mean-accumulator))
  "Mean and sum of squared error accumulator."
  (sse 0d0))

(defmethod add ((instance mean-sse-accumulator) (object number))
  (let+ (((&structure mean-sse-accumulator- tally mean sse) instance)
         (difference (- object mean)))
    (incf tally)
    (incf mean (/ difference tally))
    (incf sse (* (- object mean) difference))))

(defmethod sse ((instance mean-sse-accumulator) &optional center)
  (let+ (((&structure mean-sse-accumulator- tally mean sse) instance))
    (if center
        (+ sse (* (expt (- mean center) 2) tally))
        sse)))

(define-structure-slot-accessor mean mean-sse-accumulator :read-only? t)

(define-conforming-accumulator ((sse variance) (number number))
  (mean-sse-accumulator))

;;; covariance accumulator

(defstruct (covariance-accumulator
             (:constructor covariance-accumulator ())
             (:include tallier))
  "Accumulator for covariances."
  (x-mean 0d0)
  (y-mean 0d0)
  (cross-sum 0d0))

(defmethod add ((accumulator covariance-accumulator) (xy cons))
  (let+ (((x . y) xy)
         ((&structure covariance-accumulator- tally
                      x-mean y-mean cross-sum) accumulator)
         (y-difference (- y y-mean)))
    (incf tally)
    (incf-mean x-mean x tally)
    (incf y-mean (/ y-difference tally))
    (incf cross-sum (* (- x x-mean) y-difference))))

(defmethod covariance ((accumulator covariance-accumulator))
  (let+ (((&structure covariance-accumulator- tally cross-sum) accumulator)
         (n-1 (1- tally)))
    (when (plusp n-1)
      (/ cross-sum n-1))))

(defun covariance-xy (x y)
  "Calculate the covariance of two sequences."
  (covariance
   (with-accumulator ((covariance-accumulator) add)
     (map nil (lambda (x y) (add (cons x y))) x y))))

;;; autocovariance accumulator

(defstruct+ (autocovariance-accumulator 
             (:constructor autocovariance-accumulator%))
  "Autocovariance accumulator.  Handles missing values (NIL)."
  (n 0 :type fixnum)
  (circular-buffer nil :type vector)
  (variance-accumulator (mean-sse-accumulator))
  (covariance-accumulators nil :type vector))

(defun autocovariance-accumulator (lags)
  (autocovariance-accumulator% 
   :circular-buffer (make-array lags :initial-element nil)
   :covariance-accumulators (filled-array lags #'covariance-accumulator)))

(defgeneric lags (accumulator)
  (:documentation "Return the maximum number of available lags in
  ACCUMULATOR.")
  (:method ((accumulator autocovariance-accumulator))
    (length (autocovariance-accumulator-circular-buffer accumulator))))

(defmethod add ((accumulator autocovariance-accumulator) (x number))
  (let+ (((&autocovariance-accumulator n circular-buffer variance-accumulator
                                       covariance-accumulators) accumulator)
         (lags (length covariance-accumulators)))
    ;; add to variance
    (add variance-accumulator x)
    ;; add to covariances
    (loop for lag below lags
          for index downfrom (1- n)
          do (awhen (aref circular-buffer (mod index lags))
               (add (aref covariance-accumulators lag) (cons x it))))
    ;; move circular buffer
    (setf (aref circular-buffer (mod n lags)) x)
    (incf n)))

(defmethod add ((accumulator autocovariance-accumulator) (x null))
  ;; just move circular buffer
  (let+ (((&autocovariance-accumulator n circular-buffer nil nil) accumulator)
         (lags (length circular-buffer)))
    (setf (aref circular-buffer (mod n lags)) x)
    (incf n)))

(defmethod tally ((accumulator autocovariance-accumulator))
  (tally (autocovariance-accumulator-variance-accumulator accumulator)))

(defmethod mean ((accumulator autocovariance-accumulator))
  (mean (autocovariance-accumulator-variance-accumulator accumulator)))

(defmethod sse ((accumulator autocovariance-accumulator) &optional center)
  (let ((accumulator
         (autocovariance-accumulator-variance-accumulator accumulator)))
    (values (sse accumulator center) accumulator)))

(defgeneric autocovariances (object &optional lags)
  (:documentation "Autocovariances.")
  (:method ((accumulator autocovariance-accumulator) &optional lags)
    (subseq 
     (map 'vector #'covariance 
          (autocovariance-accumulator-covariance-accumulators accumulator))
     0 lags))
  (:method (object &optional lags)
    (let ((accumulator (autocovariance-accumulator lags)))
      (values (autocovariances (sweep accumulator object) lags) accumulator))))

(defgeneric autocorrelations (accumulator &optional lags)
  (:documentation "Autocorrelations.")
  (:method ((accumulator autocovariance-accumulator) &optional lags)
    (let+ (((&structure autocovariance-accumulator- variance-accumulator
                        covariance-accumulators) accumulator)
           (variance (variance variance-accumulator)))
      (subseq
       (map 'vector (lambda (covariance-accumulator)
                      (/ (covariance covariance-accumulator) variance))
            covariance-accumulators)
       0 lags)))
  (:method (object &optional lags)
    (let ((accumulator (autocovariance-accumulator lags)))
      (values (autocorrelations (sweep accumulator object) lags) accumulator))))

;;; sorting accumulator
;;; 
;;; This is not the most elegant way of calculating quantiles, but it will do
;;; until I implement something nicer.

(defstruct+ (sorting-accumulator
             (:constructor sorting-accumulator (&optional (predicate #'<=)))
             (:include mean-sse-accumulator))
  "Storage accumulator, simply saves elements.  When asked for ordered, order
them and return as a vector."
  (ordered-elements #() :type vector)
  (unordered-elements nil :type list)
  (predicate #'<=))

(defmethod add :after ((accumulator sorting-accumulator) object)
  (push object (sorting-accumulator-unordered-elements accumulator)))

(defgeneric elements (object)
  (:documentation "Return elements from object.  May share structure."))

(defmethod elements ((sorting-accumulator sorting-accumulator))
  (let+ (((&sorting-accumulator ordered-elements unordered-elements
                                predicate) sorting-accumulator))
    (when unordered-elements
      (setf ordered-elements (concatenate 'vector ordered-elements
                                          unordered-elements)
            unordered-elements nil
            ordered-elements (sort ordered-elements predicate))
      ordered-elements)))

(defmethod quantile ((sorting-accumulator sorting-accumulator) q)
  (let+ (((&accessors-r/o elements) sorting-accumulator))
    (assert (let+ (((&sorting-accumulator nil nil predicate) sorting-accumulator))
              (or (eq predicate #'<=)
                  (eq predicate #'<)))
            () "Accumulator has to be sorted by < or <=.")
    (let+ ((n (length elements))
           (r (* q (1- n)))
           ((&values int frac) (floor r))
           (left (aref elements int)))
      (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
      (if (zerop frac)
          left
          (convex-combination left (aref elements (1+ int)) frac)))))

(define-conforming-accumulator ((quantile quantiles) (number number))
  (sorting-accumulator))

;;; sparce accumulator arrays

(defstruct (@ (:constructor @ (object &rest subscripts)))
  "Object (observation) with given subscripts."
  (object)
  (subscripts nil :type list))

(defclass sparse-accumulator-array ()
  ((table :accessor table :initarg :table
          :initform (make-hash-table :test #'equal))
   (init-function :accessor init-function :initarg :init-function)
   (rank :accessor rank :initarg :rank))
  (:documentation "Array of accumulators.  New accumulators are created at
  particular subscripts on demand, initialized with INIT-FUNCTION."))

(defun valid-subscripts? (subscripts rank)
  "Check if subscripts are valid (list of fixnums of length RANK)."
  (let ((n 0))
    (and (every (lambda (x)
                  (incf n)
                  (typep x 'fixnum))
                subscripts)
         (= rank n))))

(defun add-with-subscripts% (instance object subscripts)
  "Function that implements (add ... (@ ...)).  Not exported"
  (bind (((:slots-r/o table init-function rank) instance)
         (subscripts (aprog1 (coerce subscripts 'list)
                       (assert (valid-subscripts? it rank))))
         ((:values accumulator present?) (gethash subscripts table)))
    (unless present?
      (setf accumulator (apply init-function subscripts)
            (gethash subscripts accumulator) accumulator))
    (add accumulator object)))

(defmethod add ((instance sparse-accumulator-array) (object @))
  (bind (((:structure @- object subscripts) object))
    (add-with-subscripts% instance object subscripts)))

;;; !!! define (add instance (@ object subscripts)) compiler macro

(defmethod ref ((instance sparse-accumulator-array) &rest subscripts)
  (bind (((:slots-r/o table rank) instance))
    (assert (valid-subscripts? subscripts rank))
    ;; !!! what happes if not found? save "touched" instances?
    (gethash subscripts table)))

(defmethod limits ((instance sparse-accumulator-array))
  (bind (((:slots-r/o table rank) instance)
         (min (make-array rank :element-type 'fixnum))
         (max (make-array rank :element-type 'fixnum)))
    (iter
      (for (subscripts nil) :in-hashtable table)
      (if (first-iteration-p)
          (progn
            (replace min subscripts)
            (replace max subscripts))
          (iter
            (for subscript :in subscripts)
            (for index :from 0)
            (maxf (aref max index) subscript)
            (minf (aref min index) subscript))))
    (values min max)))

;;; moments accumulator 

;; (defclass moments-accumulator-array (sparse-accumulator-array )
;;   ()
;;   (:documentation ""))

;; (defun moments-accumulator-array (rank &key (accumulator #'mean-accumulator))
;;   (make-instance 'moments-accumulator-array :rank rank
;;                  :init-function (lambda (&rest rest)
;;                                   (declare (ignore rest))
;;                                   (funcall accumulator))))

;;; acf accumulator

(defstruct (residual-pair (:constructor residual-pair (x x-index y y-index)))
  "Pair of residuals."
  (x)
  (x-index nil :type fixnum)
  (y)
  (y-index nil :type fixnum))

(defclass acf-accumulator (sparse-accumulator-array)
  ()
  (:documentation ""))

(defmethod add ((instance acf-accumulator) (residual-pair residual-pair))
  ;; !!! factor out part, write compiler macro
  (bind (((:structure residual-pair- x x-index y y-index) residual-pair))
    (when (<= x-index y-index)
      (add-with-subscripts% instance (* x y) (list (- y-index x-index))))))


;; (defun weighted-mean-accumulator ()
;;   "Accumulator for online calculation of the weighted mean of observations.  Called
;; with X (a real number) for accumulating the mean.  When called with no
;; arguments, return (values MEAN SW N), MEAN is double-float, SW (sum of weights) is
;; double-float, N is fixnum."
;;   ;; West (1979)
;;   (let ((n 0)
;;         (mean 0)
;;         (sw 0))
;;     (lambda (&optional x w)
;;       (if x
;;           (progn
;;             (incf n)
;;             (incf sw w)
;;             (incf mean (/ (* (- x mean) w) sw)))
;;           (values mean sw n)))))


;; (defun weighted-sse-accumulator ()
;;   "Accumulator for weighted sum of squared errors.  When called without arguments,
;; return (values SSE SW MEAN N), where SW is the sum of weights."
;;   ;; West (1979)
;;   (declare (optimize speed))
;;   (let ((n 0)
;;         (mean 0d0)
;;         (sse 0d0)
;;         (sw 0d0))
;;     (declare (fixnum n))
;;     (lambda (&optional x w)
;;       (if x
;;           (let ((previous-sw sw))
;;             (incf n)
;;             (incf sw w)
;;             (let* ((q (- x mean))
;;                    (r (/ (* q w) sw)))
;;               (incf sse (* previous-sw q r))
;;               (incf mean r)))
;;           (values sse sw mean n)))))

;; (defgeneric weighted-mean (object weights)
;;   (:documentation "Return the weighted sample mean.")
;;   (:method ((sequence sequence) (weights sequence))
;;     (apply-accumulator (weighted-mean-accumulator) sequence weights)))

;; (defgeneric weighted-variance (object weights)
;;   (:documentation "Return the weighted sample mean.  If there are second and third
;;   values, they are the mean and the sum of weights.")
;;   (:method ((sequence sequence) (weights sequence))
;;     (bind (((:values sse sw mean)
;;             (apply-accumulator (weighted-sse-accumulator) sequence weights)))
;;       (values (/ sse (1- sw)) mean))))

;;; !!! todo: mean and variance for matrices
;;;           covariance (by stacking vectors?)
;;;           correlation (matrix)

;; (defun sample-cov (a b &key (a-mean (mean a)) (b-mean (mean b)))
;;   "Sample covariance between A and B.  Means will be used when provided."
;;   (let* ((a (coerce a 'vector))
;;          (b (coerce b 'vector))
;;          (size (length a))
;;          (sum 0))
;;     (assert (= (length b) size) () "Vectors do not have the same length.")
;;     (dotimes (i size)
;;       (incf sum (* (- (aref a i) a-mean)
;;                    (- (aref b i) b-mean))))
;;     (sample-second-moment% sum size)))

;; (defun sample-corr (a b &key (a-mean (mean a)) (b-mean (mean b)))
;;   "Sample correlation between A and B.  Means will be used when provided."
;;   (/ (sample-cov a b :a-mean a-mean :b-mean b-mean)
;;      (sample-sd a a-mean) (sample-sd b b-mean)))

;;; sensible behavior for sequences and arrays

;; (defmethod size ((sequence sequence))
;;   (length sequence))

;; (defmethod size ((array array))
;;   (array-total-size array))

;; (defmethod sum ((sequence sequence))
;;   (reduce #'+ sequence))

;; (defmethod sum ((array array))
;;   (iter
;;     (for index :from 0 :below (array-total-size array))
;;     (summing (row-major-aref array index))))

;; (defmethod sse ((sequence sequence) &optional (mean (mean sequence)))
;;   (reduce #'+ sequence :key (lambda (x) (expt (- x mean) 2))))

;; (defmethod sse ((array array) &optional (mean (mean array)))
;;   (iter
;;     (for index :from 0 :below (array-total-size array))
;;     (summing (expt (- (row-major-aref array index) mean) 2))))

(defun subranges (ranges)
  "Given a sequence of integer ranges with elements (start . end),
return (values SUBRANGES INDEX-LISTS).  SUBRANGES is a vector of subranges
with elements (start . end), and INDEX-LISTS is a vector of lists, enumerating
the subintervals that make up the corresponding original interval.  All
returned ranges and indexes are in increasing order, but RANGES doesn't have
to be.

Example:

  (subranges #((0 . 100) (50 . 150)))

  evaluates to values

    #((0 . 50) (50 . 100) (100 . 150))
    #((0 1) (1 2))

The algorithm makes sure that only subranges which are in a nonempty range are
kept.  For example,

  (subranges #((0 . 50) (100 . 150)))

  evaluates to values

    #((0 . 50) (100 . 150))
    #((0) (1))

When (>= start end), a range is considered empty.  If all ranges are empty,
SUBRANGES is #() and INDEX-LISTS contains NILs.  For example,

  (subranges #((5 . 0) (6 . 6)))

  evaluates to values

    #()
    #(NIL NIL)
"
  (declare (optimize debug))
  (let* ((ranges (coerce ranges 'vector))
         (endpoints (coerce (remove-duplicates 
                             (iter
                               (for (start . end) :in-vector ranges)
                               (when (< start end)
                                 (collect start)
                                 (collect end))))
                            'simple-fixnum-vector)))
    ;; if all ranges are empty, there are no subranges
    (unless (plusp (length endpoints))
      (return-from subranges (values #()
                                     (make-array (length ranges)
                                                 :initial-element nil))))
    ;; sort endpoints
    (let* ((endpoints (sort endpoints #'<=))
           (within-lists (make-array (1- (length endpoints))
                                     :initial-element nil)))
      ;; check which subintervals are within any range
      (iter
        (for (start . end) :in-vector ranges :with-index range-index)
        (when (< start end)
          (iter
            (for endpoint :from (binary-search endpoints start)
                          :below (binary-search endpoints end))
            (push range-index (aref within-lists endpoint)))))
      ;; remove those that are not in use
      (iter
        (for end :in-vector endpoints :from 1)
        (for start :previous end :initially (first* endpoints))
        (for within-list :in-vector within-lists)
        (when within-list
          (collect (cons start end) :into subranges :result-type vector)
          (collect (nreverse within-list) :into within-lists2))
        (finally
         ;; for each range, list constituent subranges
         (let ((subrange-lists (make-array (length ranges)
                                           :initial-element nil)))
           (iter
             (for within-list :in within-lists2)
             (for subrange-index :from 0)
             (dolist (within-index within-list)
               (push subrange-index (aref subrange-lists within-index))))
           (return (values subranges
                           (map 'vector #'nreverse subrange-lists)))))))))
