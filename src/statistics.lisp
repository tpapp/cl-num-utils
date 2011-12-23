;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Generic interface

(defgeneric add (accumulator object)
  (:documentation "Add accumulator to object.  NILs are ignored by the
  accumulator, unless a specialized method decides otherwise.")
  (:method (accumulator (object null))))

(defgeneric pool2 (accumulator1 accumulator2)
  (:documentation "Pool two accumulators.  When they are of a different type,
  the resulting accumulator will be downgraded to the level afforded by the
  information available in the accumulators."))

(defun pool* (accumulators)
  "Pool ACCUMULATORS."
  (reduce #'pool2 accumulators))

(defun pool (&rest accumulators)
  "Pool ACCUMULATORS."
  (declare (inline pool*))
  (pool* accumulators))

;;; !! write compiler macro for (pool acc1 acc2) => (pool2 acc1 acc2)

(defmacro define-default-add (accumulator &key (ignore-nil? t))
  "This macro is used to define default ADD methods so that we don't default
to a superclass.  When IGNORE-NIL?, NILs are silently ignored."
  `(progn
     (defmethod add ((accumulator ,accumulator) object)
       (error "Accumulator does not handle objects of this type."))
     ,(when ignore-nil?
        `(defmethod add ((accumulator ,accumulator) (object null))))))

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

(defgeneric sweep (accumulator object &key key)
  (:documentation "Apply ACCUMULATOR to elements of OBJECT.  When ACCUMULATOR
  is a function, it is used to generate a conforming accumulator.")
  (:method (accumulator (sequence sequence) &key (key #'identity))
    (with-accumulator (accumulator add)
      (map nil (compose #'add key) sequence)))
  (:method (accumulator (array array) &key (key #'identity))
    (with-accumulator (accumulator add)
      (map nil (compose #'add key) (flatten-array array)))))

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

(defgeneric sample-ratio (object)
  (:documentation "Return the proportion of non-nil elements.")
  (:method (object)
    (let ((accumulator (sweep (sample-ratio-accumulator) object)))
      (values (sample-ratio accumulator) accumulator))))

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

(defgeneric sd (object)
  (:documentation "Return the standard deviation.")
  (:method (object)
    (sqrt (variance object))))

(defgeneric quantile (object q)
  (:documentation "Return an element at quantile Q.  May be an interpolation
  or an approximation, depending on OBJECT and Q.")
  (:method (object q)
    (let ((accumulator (sweep 'quantiles object)))
      (values (quantile accumulator q) accumulator))))

(defgeneric quantiles (object qs)
  (:documentation "Multiple quantiles, see QUANTILE.")
  (:method (object qs)
    (let ((accumulator (sweep 'quantiles object)))
      (values (map1 (lambda (q) (quantile accumulator q)) qs)
              accumulator))))

(defgeneric median (object)
  (:documentation "Median of OBJECT.")
  (:method ((object sequence))
    (alexandria:median object))
  (:method (object)
    (quantile object 0.5)))

;;; Specific accumulators

;;; tallier

(defstruct (tallier (:constructor tallier ()))
  (tally 0 :type fixnum))

(defmethod add ((tallier tallier) object)
  (incf (tallier-tally tallier)))

(define-structure-slot-accessor tally tallier :read-only? t)

(define-conforming-accumulator (tally object)
  (tallier))

(defmethod == ((a tallier) (b tallier) &optional tolerance)
  (declare (ignore tolerance))
  (= (tallier-tally a) (tallier-tally b)))

;;; sample ratio

(defstruct (sample-ratio-accumulator 
            (:constructor sample-ratio-accumulator ())
            (:include tallier))
  "Sample ratio accumulator."
  (count 0 :type fixnum))

(defmethod add ((accumulator sample-ratio-accumulator) object)
  (let+ (((&structure sample-ratio-accumulator- tally count) accumulator))
    (incf tally)
    (when object
      (incf count))))

(defmethod sample-ratio ((accumulator sample-ratio-accumulator))
  (let+ (((&structure-r/o sample-ratio-accumulator- tally count) accumulator))
    (/ count tally)))

;;; mean accumulator for scalars

(defstruct (mean-accumulator
             (:include tallier)
             (:constructor mean-accumulator (&optional (tally 0) (mean 0d0))))
  "Accumulator for a (scalar) mean."
  (mean 0d0))

(define-default-add mean-accumulator)

(define-modify-macro incf-mean (value tally)
  (lambda (mean value tally) (+ mean (/ (- value mean) tally)))
  "When MEAN is the MEAN of (1- TALLY) numbers, update it with VALUE.  In
practice, TALLY should be INCF'd before using incf-mean.")

(defmethod add ((instance mean-accumulator) (object number))
  (let+ (((&structure mean-accumulator- tally mean) instance))
    (incf tally)
    (incf-mean mean object tally)))

(define-structure-slot-accessor mean mean-accumulator :read-only? t)

(define-conforming-accumulator (mean (number number))
  (mean-accumulator))

(declaim (inline pooled-mean))
(defun pooled-mean (tally1 mean1 tally2 mean2
                    &optional (tally (+ tally1 tally2)))
  "Pooled mean.  For internal use."
  (/ (+ (* tally1 mean1) (* tally2 mean2)) tally))

(defmethod pool2 ((acc1 mean-accumulator) (acc2 mean-accumulator))
  (let+ (((&structure mean-accumulator- (tally1 tally) (mean1 mean)) acc1)
         ((&structure mean-accumulator- (tally2 tally) (mean2 mean)) acc2)
         (tally (+ tally1 tally2)))
    (mean-accumulator tally (pooled-mean tally1 mean1 tally2 mean2 tally))))

(defmethod == ((acc1 mean-accumulator) (acc2 mean-accumulator)
               &optional (tolerance *==-tolerance*))
  (let+ (((&structure mean-accumulator- (tally1 tally) (mean1 mean)) acc1)
         ((&structure mean-accumulator- (tally2 tally) (mean2 mean)) acc2))
    (and (= tally1 tally2)
         (== mean1 mean2 tolerance))))

;;; mean accumulator for arrays

(defstruct (array-mean-accumulator
             (:constructor array-mean-accumulator% (mean)))
  "Array of accumulators."
  (tally 0 :type fixnum)
  (mean nil :type array :read-only t))

(define-structure-let+ (array-mean-accumulator) tally mean)

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
             (:constructor mean-sse-accumulator
              (&optional (tally 0) (mean 0d0) (sse 0d0)))
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

(declaim (inline pooled-sse))
(defun pooled-sse (tally1 mean1 sse1 tally2 mean2 sse2
                   &optional (tally (+ tally1 tally2)))
  (+ sse1 sse2
     (/ (* tally1 tally2 (expt (- mean2 mean1) 2))
        tally)))

(defmethod pool2 ((acc1 mean-sse-accumulator) (acc2 mean-sse-accumulator))
  (let+ (((&structure-r/o mean-sse-accumulator- (tally1 tally) (mean1 mean)
                          (sse1 sse)) acc1)
         ((&structure-r/o mean-sse-accumulator- (tally2 tally) (mean2 mean)
                          (sse2 sse)) acc2)
         (tally (+ tally1 tally2)))
    (mean-sse-accumulator tally
                          (pooled-mean tally1 mean1 tally2 mean2 tally)
                          (pooled-sse tally1 mean1 sse1 tally2 mean2 sse2
                                      tally))))

(defmethod == ((acc1 mean-sse-accumulator) (acc2 mean-sse-accumulator)
               &optional (tolerance *==-tolerance*))
  (let+ (((&structure-r/o mean-sse-accumulator- (tally1 tally) (mean1 mean)
                          (sse1 sse)) acc1)
         ((&structure-r/o mean-sse-accumulator- (tally2 tally) (mean2 mean)
                          (sse2 sse)) acc2))
    (and (= tally1 tally2)
         (== mean1 mean2 tolerance)
         (== sse1 sse2 tolerance))))

;;; covariance accumulator

(defstruct (covariance-accumulator
             (:constructor covariance-accumulator ())
             (:include tallier))
  "Accumulator for covariances."
  (x-mean 0d0)
  (x-sse 0d0)
  (y-mean 0d0)
  (y-sse 0d0)
  (cross-sse 0d0))

(define-default-add covariance-accumulator)

(define-structure-let+ (covariance-accumulator)
    x-mean x-sse y-mean y-sse cross-sse)

(defmethod add ((accumulator covariance-accumulator) (xy cons))
  (let+ (((x . y) xy)
         ((&structure covariance-accumulator- tally
                      x-mean y-mean x-sse y-sse cross-sse) accumulator)
         (x-delta (- x x-mean))
         (y-delta (- y y-mean)))
    (incf tally)
    (incf x-mean (/ x-delta tally))
    (incf y-mean (/ y-delta tally))
    (let ((x-post-delta (- x x-mean))
          (y-post-delta (- y y-mean)))
      (incf x-sse (* x-delta x-post-delta))
      (incf y-sse (* y-delta y-post-delta))
      (incf cross-sse (* x-post-delta y-delta)))))

(defmethod covariance ((accumulator covariance-accumulator))
  (let+ (((&structure covariance-accumulator- tally cross-sse) accumulator)
         (n-1 (1- tally)))
    (when (plusp n-1)
      (/ cross-sse n-1))))

(defmethod correlation ((accumulator covariance-accumulator))
  (let+ (((&structure covariance-accumulator- x-sse y-sse cross-sse
                      tally) accumulator)
         (denominator (sqrt (* x-sse y-sse))))
    (cond
      ((plusp denominator) (/ cross-sse denominator))
      ((plusp tally) 0)
      (t nil))))

(defun sweep-xy (accumulator x y)
  "Sweep sequences x and y with an accumulator that takes conses."
  (with-accumulator (accumulator add)
    (map nil (lambda (x y) (add (cons x y))) x y)))

(defun covariance-xy (x y)
  "Calculate the covariance of two sequences."
  (let ((acc (sweep-xy (covariance-accumulator) x y)))
    (values (covariance acc) acc)))

(defun correlation-xy (x y)
  "Calculate the correlation of two sequences."
  (let ((acc (sweep-xy (covariance-accumulator) x y)))
    (values (correlation acc) acc)))

(defmethod == ((acc1 covariance-accumulator) (acc2 covariance-accumulator)
               &optional (tolerance *==-tolerance*))
  (let+ (((&covariance-accumulator x-mean1 x-sse1 y-mean1 y-sse1
                                   cross-sse1) acc1)
         ((&covariance-accumulator x-mean2 x-sse2 y-mean2 y-sse2
                                   cross-sse2) acc2))
    (and (= (tally acc1) (tally acc2))
         (== x-mean1 x-mean2 tolerance)
         (== x-sse1 x-sse2 tolerance)
         (== y-mean1 y-mean2 tolerance)
         (== y-sse1 y-sse2 tolerance)
         (== cross-sse1 cross-sse2 tolerance))))

;;; autocovariance accumulator

(defstruct (autocovariance-accumulator 
             (:constructor autocovariance-accumulator%))
  "Autocovariance accumulator.  Handles missing values (NIL)."
  (circular-buffer nil :type list)
  (covariance-accumulators nil :type vector))

(define-default-add autocovariance-accumulator)

(defun autocovariance-accumulator (lags)
  "Create an autocovariance accumulator with a given number of lags."
  (autocovariance-accumulator% 
   :circular-buffer (make-circular-list lags :initial-element nil)
   :covariance-accumulators (filled-array lags #'covariance-accumulator)))

(defgeneric lags (accumulator)
  (:documentation "Return the maximum number of available lags in
  ACCUMULATOR.")
  (:method ((accumulator autocovariance-accumulator))
    (length (autocovariance-accumulator-covariance-accumulators
             accumulator))))

(defmethod add ((accumulator autocovariance-accumulator) (x number))
  (let+ (((&structure autocovariance-accumulator- circular-buffer 
                      covariance-accumulators) accumulator))
    ;; update covariances
    (iter
      (for cov-acc :in-vector covariance-accumulators :downto 0)
      (for elt :in circular-buffer)
      (when elt
         (add cov-acc (cons x elt))))
    ;; save element and update circular buffer
    (setf (car circular-buffer) x
          circular-buffer (cdr circular-buffer))))

(defmethod add ((accumulator autocovariance-accumulator) (x null))
  ;; just move circular buffer
  (let+ (((&structure autocovariance-accumulator- circular-buffer)
          accumulator))
    ;; save element and update circular buffer
    (setf (car circular-buffer) nil
          circular-buffer (cdr circular-buffer))))

(defgeneric autocovariances (object &optional lags)
  (:documentation "Autocovariances.")
  (:method ((accumulator autocovariance-accumulator) &optional lags)
    (subseq 
     (map1 #'covariance 
           (autocovariance-accumulator-covariance-accumulators accumulator))
     0 lags))
  (:method (object &optional lags)
    (let ((accumulator (autocovariance-accumulator lags)))
      (values (autocovariances (sweep accumulator object) lags)
              accumulator))))

(defgeneric autocorrelations (object &optional lags)
  (:documentation "Vector of autocorrelations.")
  (:method ((accumulator autocovariance-accumulator) &optional lags)
    (subseq 
     (map1 #'correlation
           (autocovariance-accumulator-covariance-accumulators accumulator))
     0 lags))
  (:method (object &optional lags)
    (let ((accumulator (autocovariance-accumulator lags)))
      (values (autocorrelations (sweep accumulator object) lags)
              accumulator))))

(defmethod == ((acc1 autocovariance-accumulator)
               (acc2 autocovariance-accumulator)
               &optional (tolerance *==-tolerance*))
  (== (autocovariance-accumulator-covariance-accumulators acc1)
      (autocovariance-accumulator-covariance-accumulators acc2)
      tolerance))

(defmethod print-object ((object autocovariance-accumulator) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (object stream :type t)
        (format stream "autocorrelations: ~A" (autocorrelations object)))))

;;; sorting accumulator
;;; 
;;; This is not the most elegant way of calculating quantiles, but it will do
;;; until I implement something nicer.

(defstruct sorted-reals
  "Accumulator which sorts elements.  ELEMENTS return the sorted elements."
  (ordered-elements #() :type vector)
  (unordered-elements nil :type list))

(define-structure-let+ (sorted-reals)
                       ordered-elements unordered-elements)

(defmethod add ((accumulator sorted-reals) object)
  (push object (sorted-reals-unordered-elements accumulator)))

(defmethod elements ((sorted-reals sorted-reals))
  (let+ (((&sorted-reals ordered-elements unordered-elements) sorted-reals))
    (when unordered-elements
      (setf ordered-elements (concatenate 'vector ordered-elements
                                          unordered-elements)
            unordered-elements nil
            ordered-elements (sort ordered-elements #'<)))
    ordered-elements))

(defun empirical-quantile (sorted-vector q)
  "Return the empirical quantile of a vector of real numbers, sorted in
ascending order (not checked).  Uses a 0.5 correction."
  (let+ ((n (length sorted-vector))
         (c (/ 1/2 n)))
    (cond
      ((or (< q 0) (< 1 q)) (error "Quantile ~A is not in [0,1]." q))
      ((<= q c) (aref sorted-vector 0))
      ((<= (- 1 c) q) (aref sorted-vector (1- n)))
      (t (let+ ((r (- (* q n) 1/2))
                ((&values int frac) (floor r))
                ((&flet value (index)
                   (aref sorted-vector index)))
                (left (value int)))
           (if (zerop frac)
               left
               (convex-combination left (value (1+ int)) frac)))))))

(defmethod quantile ((accumulator sorted-reals) q)
  (empirical-quantile (elements accumulator) q))

(defun sorted-reals ()
  (make-sorted-reals))

(define-conforming-accumulator ((quantile quantiles) (number number))
  (sorted-reals))

(defun sort-reals (sequence)
  "Return a SORTED-REALS structure."
  (make-sorted-reals :ordered-elements (sort (copy-sequence 'vector sequence)
                                             #'<)
                     :unordered-elements nil))

(defmethod print-object ((acc sorted-reals) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (acc stream :type t)
        (let+ (((&accessors-r/o elements) acc))
          (if (plusp (length elements))
              (format stream "min: ~A, q25: ~A, q50: ~A, q75: ~A, max: ~A"
                      (first* elements)
                      (quantile acc 0.25)
                      (quantile acc 0.5)
                      (quantile acc 0.75)
                      (sub elements -1))
              (format stream "no elements"))))))

(defgeneric ensure-sorted-reals (object)
  (:documentation "Return the contents of OBJECT as a SORTED-REALS.")
  (:method ((sorted-reals sorted-reals))
    sorted-reals)
  (:method ((array array))
    (sort-reals (flatten-array array)))
  (:method ((list list))
    (sort-reals list)))

;;; sparse accumulator arrays

(defstruct (at (:constructor at (object &rest subscripts)))
  "Object (observation) with given subscripts."
  (object)
  (subscripts nil :type list))

(defstruct sparse-accumulator-array
  "See documentation of the function SPARSE-ACCUMULATOR-ARRAY."
  (table (make-hash-table :test #'equal) :type hash-table :read-only t)
  (generator nil :type function :read-only t)
  (rank nil :type fixnum :read-only t)
  (limits nil :type list))

(defun sparse-accumulator-array (rank generator)
  "Create a sparse accumulator array.  Elements are added with subscripts (see
the function AT), and GENERATOR is called to generate an accumulator when the
first element is added with given subscripts.  Use LIMITS and REF to access
the result."
  (make-sparse-accumulator-array :generator generator :rank rank))

(defun valid-subscripts? (subscripts rank)
  "Check if subscripts are valid (list of fixnums of length RANK)."
  (let ((n 0))
    (and (every (lambda (x)
                  (incf n)
                  (typep x 'fixnum))
                subscripts)
         (= rank n))))

(defun sparse-accumulator-ref% (sparse-accumulator-array subscripts
                                &optional save-new?)
  "Return the accumulator corresponding to subscripts, or create one on demand
when there isn't one.  When SAVE-NEW?, the latter is saved to the array,
otherwise it isn't."
  (let+ (((&structure-r/o sparse-accumulator-array- table generator rank)
          sparse-accumulator-array)
         ((&assert (valid-subscripts? subscripts rank)))
         ((&values accumulator present?) (gethash subscripts table)))
    (unless present?
      (setf accumulator (funcall generator))
      (when save-new?
        (setf (gethash subscripts table) accumulator)
        (let+ (((&structure sparse-accumulator-array- limits)
                sparse-accumulator-array))
          (if limits
              (map nil (lambda (l s)
                         (minf (car l) s)
                         (maxf (cdr l) (1+ s)))
                   limits subscripts)
              (setf limits (mapcar (lambda (s) (cons s s)) subscripts))))))
    accumulator))

(defmethod add ((instance sparse-accumulator-array) (object at))
  (add (sparse-accumulator-ref% instance (at-subscripts object) t)
       (at-object object)))



;;; !!! define (add instance (at object subscripts)) compiler macro

(defgeneric ref (object &rest subscripts)
  (:documentation "Generalization of AREF."))

(defmethod ref ((instance sparse-accumulator-array) &rest subscripts)
  (sparse-accumulator-ref% instance subscripts))

(defmethod limits ((instance sparse-accumulator-array))
  (sparse-accumulator-array-limits instance))

(defmethod as-array ((object sparse-accumulator-array) &key once-only? copy?)
  (assert (not copy?) () "Copying is not implemented.")
  (let+ (((&structure-r/o sparse-accumulator-array- table generator limits)
          object)
         (offset (mapcar #'car limits))
         (dimensions (mapcar (lambda (c) (- (cdr c) (car c))) limits))
         (array (filled-array dimensions
                              (if once-only?
                                  (funcall generator)
                                  generator))))
    (maphash (lambda (key value)
               (setf (apply #'aref array (mapcar #'- key offset)) value))
             table)
    (values array offset)))

(defmethod keys-and-values ((object sparse-accumulator-array))
  (keys-and-values (sparse-accumulator-array-table object)))

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

;; (defstruct (residual-pair (:constructor residual-pair (x x-index y y-index)))
;;   "Pair of residuals."
;;   (x)
;;   (x-index nil :type fixnum)
;;   (y)
;;   (y-index nil :type fixnum))

;; (defclass acf-accumulator (sparse-accumulator-array)
;;   ()
;;   (:documentation ""))

;; (defmethod add ((instance acf-accumulator) (residual-pair residual-pair))
;;   ;; !!! factor out part, write compiler macro
;;   (let+ (((&structure residual-pair- x x-index y y-index) residual-pair))
;;     (when (<= x-index y-index)
;;       (add-with-subscripts% instance (* x y) (list (- y-index x-index))))))


;;; Histograms
;;; 
;;; data structures for counting the frequencies of multivariate indexes
;;; (always of the same rank).  Building block for a histogram, but does not
;;; have information on where the indexes came from.  Frequencies can only be
;;; added, not set or subtracted.  The total is available, but is not
;;; necessarily cached/saved.  Some combinations of subscripts may be invalid
;;; or unavailable, in that case an error is raised.
;;; 
;;; !! define condition
;;; !! write methods for using an array as frequencies

(defstruct (histogram-accumulator
            (:include sparse-accumulator-array))
  bins)

(defmethod bins ((accumulator histogram-accumulator))
  (histogram-accumulator-bins accumulator))

(defun histogram-accumulator (&rest bins)
  (make-histogram-accumulator :bins bins :generator #'tallier
                              :rank (length bins)))

(defmethod add ((accumulator histogram-accumulator) (sequence sequence))
  (let+ (((&structure-r/o histogram-accumulator- bins) accumulator))
    (assert (= (length sequence) (length bins)))
    (add accumulator (apply #'at 1 (map 'list #'bin-index bins sequence)))))

(defmethod add ((accumulator histogram-accumulator) (number number))
  (add accumulator (list number)))

(defmethod keys-and-values ((accumulator histogram-accumulator))
  (map 'vector (lambda+ ((k . v))
                 (cons k (tally v)))
       (call-next-method)))

(defun locations-and-tallies (histogram-accumulator)
  (let+ (((&structure-r/o histogram-accumulator- bins) histogram-accumulator))
    (map 'vector (lambda+ ((k . v))
                   (cons (mapcar (lambda (b i) (bin-location b i))
                                 bins k)
                         v))
         (keys-and-values histogram-accumulator))))

(defun location-limits (histogram-accumulator)
  (mapcar (lambda (l b)
            (limits (list (bin-location b (car l))
                          (bin-location b (cdr l)))))
          (limits histogram-accumulator)
          (bins histogram-accumulator)))

(defmethod ref ((accumulator histogram-accumulator) &rest subscripts)
  (tally (apply #'call-next-method accumulator subscripts)))

(defparameter *frequency-print-width* 40
  "The number of columns used for printing frequencies using text symbols.
  Does not include the space used by labels etc.")

(defun print-univariate-frequencies% (stream labels-frequencies)
  "Print univaritate frequencies from TABLE to stream.  LABELS-FREQUENCIES
should be a vector of sorted (LABEL . FREQUENCY) conses.  Not exported."
  (iter
    (for (label . frequency) :in labels-frequencies)
    (maximize (length label) :into label-width)
    (maximize frequency :into maximum-frequency)
    (finally 
     (let ((step (max (pretty-step maximum-frequency *frequency-print-width*)
                      1)))
       (loop for (label . frequency) in labels-frequencies
             do (format stream "~&~vA " label-width label)
                (let+ (((&values quotient remainder) (floor frequency step)))
                  (loop repeat quotient do (princ #\* stream))
                  (when (plusp remainder) (princ #\. stream))))
       (format stream "~&(each * = frequency of ~A)" step)))))

(defmethod print-object ((accumulator histogram-accumulator) stream)
  (let+ (((&accessors-r/o limits) accumulator))
    (if (= (length limits) 1)
        (let+ ((((start . end)) limits)
               ((bin) (bins accumulator))
               (labels-frequencies
                (iter
                  (for index :from start :below end)
                  (collect (cons (format-bin-location (bin-location bin index))
                                 (ref accumulator index))))))
          (print-unreadable-object (accumulator stream :type t)
            (print-univariate-frequencies% stream labels-frequencies)))
        (call-next-method))))

(defun scott-rule (object &key (pretty? t) (correction 1d0) offset)
  "Return a bin object using Scott's (1979) rule for bin width, multiplied by
CORRECTION, rounded when PRETTY?.  When OFFSET is given, it is used for the
bins, otherwise they are centered around 0."
  (let+ (((&accessors-r/o tally sd) (sweep 'sse object))
         (width (* 3.5d0 (expt tally -1/3) sd correction))
         (width (if pretty?
                    (pretty width)
                    width)))
    (even-bins width (aif offset it (/ width 2)))))

(defun histogram1 (object &optional (bins #'scott-rule))
  "Return a univariate histogram of OBJECT.  If BINS is a function, call it on
object to generate the bins."
  (sweep (histogram-accumulator (if (functionp bins)
                                    (funcall bins object)
                                    bins))
         object))

;; (defun histogram-from-matrix (matrix &rest bins)
;;   (let+ ((histogram (apply #'make-hashed-histogram bins))
;;          ((nrow ncol) (array-dimensions matrix)))
;;     (assert (= (subscript-rank histogram) ncol))
;;     (loop for row :below nrow do
;;       (apply #'add-observation histogram
;;              1 (coerce (subarray matrix row) 'list)))
;;     histogram))

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

(defun subranges (ranges &key shadow-ranges)
  "Given a sequence of integer ranges with elements (start . end),
return (values SUBRANGES INDEXES-LISTS).  SUBRANGES is a vector of subranges
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

Shadow-ranges are ranges which are taken into account when calculating the
subranges, but have no corresponding elements in the returned vectors.  For
example, :shadow-ranges '(0 . 1000) will make subranges superset of a
partition of [0 ... 999]."
  (let+ ((n (length shadow-ranges))
         (ranges (concatenate 'vector shadow-ranges ranges))
         ((&values subranges index-lists) (subranges% ranges)))
    (values subranges (subseq index-lists n))))

(defun subranges% (ranges)
  "Internal function used by subranges."
  (let* ((endpoints (coerce (remove-duplicates 
                             (iter
                               (for (start . end) :in-vector ranges)
                               (when (< start end)
                                 (collect start)
                                 (collect end))))
                            'simple-fixnum-vector)))
    ;; if all ranges are empty, there are no subranges
    (unless (plusp (length endpoints))
      (return-from subranges% (values #()
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

(defun demean (object &optional (mean (mean object)))
  "Subtract mean from object.  The mean is returned as the second value."
  (values (e- object mean) mean))

;;; references
;;; 
;; @inproceedings{bennett2009numerically,
;;   title={Numerically stable, single-pass, parallel statistics algorithms},
;;   author={Bennett, J. and Grout, R. and P{\'e}bay, P. and Roe, D. and Thompson, D.},
;;   booktitle={Cluster Computing and Workshops, 2009. CLUSTER'09. IEEE International Conference on},
;;   pages={1--8},
;;   year={2009},
;;   organization={IEEE}
;; }
;;
;; @article{west1979updating,
;;   title={Updating mean and variance estimates: An improved method},
;;   author={West, DHD},
;;   journal={Communications of the ACM},
;;   volume={22},
;;   number={9},
;;   pages={532--535},
;;   year={1979},
;;   publisher={ACM}
;; }
