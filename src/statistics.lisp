;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
(cl:defpackage #:cl-num-utils.statistics
  (:nicknames #:clnu.stats)
  (:use #:cl
        #:anaphora
        #:alexandria
        #:cl-num-utils.arithmetic
        #:cl-num-utils.num=
        #:cl-num-utils.utilities
        #:let-plus)
  (:shadow #:mean
           #:variance
           #:median)
  (:export
   #:tally
   #:add
   #:pool
   #:empty-accumulator
   #:not-enough-elements-in-accumulator
   #:information-not-collected-in-accumulator
   #:central-sample-moments
   #:central-sample-moments-degree
   #:*central-sample-moments-default-degree*
   #:mean
   #:variance
   #:sd
   #:central-m2
   #:central-m3
   #:central-m4
   #:skewness
   #:kurtosis
   #:median
   #:sorted-reals
   #:sorted-reals-elements
   #:empirical-quantile
   #:empirical-quantile-probabilities
   #:quantile
   #:quantiles
   #:ensure-sorted-reals
   #:ensure-sorted-vector
   #:weighted-quantiles
   #:make-sparse-counter
   #:sparse-counter
   #:sparse-counter-count
   #:sparse-counter-table
   #:tabulate
   #:cross-tabulate))

(in-package #:cl-num-utils.statistics)

;;; Generic interface for accumulators
;;;
;;; Accumulators are used for one-pass calculation of moments.

(defgeneric tally (accumulator)
  (:documentation "The total weight of elements in ACCUMULATOR."))

(defgeneric add (accumulator object &key)
  (:documentation "Add OBJECT to ACCUMULATOR.  Return OBJECT.  NILs are ignored by the accumulator, unless a specialized method decides otherwise.  Keywords may be used to specify additional information (eg weight).")
  (:method (accumulator (object null) &key)
    object))

(defgeneric pool2 (accumulator1 accumulator2)
  (:documentation "Pool two accumulators.  When they are of a different type, the resulting accumulator will be downgraded to the level afforded by the information available in the accumulators."))

(defun pool (&rest accumulators)
  "Pool ACCUMULATORS."
  (reduce #'pool2 accumulators))

(define-condition empty-accumulator (error)
  ())

(define-condition not-enough-elements-in-accumulator (error)
  ())

(define-condition information-not-collected-in-accumulator (error)
  ())

(defstruct tally-mixin
  "Mixin structure that contains a tally.  Not exported.  W is the total weight."
  (w 0 :type (real 0)))

(defmethod tally ((accumulator tally-mixin))
  (tally-mixin-w accumulator))

;;; Central moments
;;;
;;; Sample central moments (and relates measures, such as skewness and
;;; kurtosis) are calculated using a one-pass algorithm, with the results
;;; accumulated in a CENTRAL-SAMPLE-MOMENTS object.

(defstruct (central-sample-moments (:include tally-mixin))
  "Central sample moments calculated on-line/single-pass.

   M   weighted mean
   S2  weighted sum of squared deviations from the mean, not calculated when NIL
   S3  weighted sum of cubed deviations from the mean, not calculated when NIL
   S4  weighted sum of 4th power deviations from the mean, not calculated when NIL

Allows on-line, numerically stable calculation of moments.  See
\cite{bennett2009numerically} and \cite{pebay2008formulas} for the description
of the algorithm.  M_2, ..., M_4 in the paper are s2, ..., s4 in the code."
  (m 0d0 :type real)
  (s2 0d0 :type (or (real 0) null))
  (s3 0d0 :type (or real null))
  (s4 0d0 :type (or (real 0) null)))

(define-structure-num= central-sample-moments w m s2 s3 s4)

(defmethod add ((moments central-sample-moments) (y real) &key (weight 1))
  ;; NOTE: See the docstring of CENTRAL-SAMPLE-MOMENTS for the description of
  ;; the algorithm.
  (assert (<= 0 weight) () "Algorithm is only stable with nonnegative weights.")
  (when (plusp weight)
    (let+ ((y (coerce y 'double-float))
           ((&structure central-sample-moments- (wa w) m s2 s3 s4) moments)
           (d (- y m))
           (w (+ wa weight))
           (d/w (/ d w))
           (d-weighted (* d/w weight)))
      (incf m d-weighted)
      (when s2
        (let ((s2-increment (* d-weighted wa d)))
          (when s3
            (let* ((x1 (* 3 d-weighted s2))
                   (x2 (* d/w s2-increment))
                   (s3-increment (- (* x2 (- wa weight)) x1)))
              (when s4
                (incf s4 (+ (* -4 s3 d-weighted)
                            (* 2 x1 d-weighted)
                            (* x2 d/w (+ (expt weight 2) (* wa (- wa weight)))))))
              (incf s3 s3-increment)))
          (incf s2 s2-increment)))
      (setf wa w)))
  y)

(defmethod pool2 ((moments-a central-sample-moments)
                  (moments-b central-sample-moments))
  (let+ (((&structure-r/o central-sample-moments- (wa w) (ma m) (s2a s2)
                         (s3a s3) (s4a s4)) moments-a)
         ((&structure-r/o central-sample-moments- (wb w) (mb m) (s2b s2)
                         (s3b s3) (s4b s4)) moments-b)
         (w (+ wa wb))
         (wab (* wa wb))
         (d (- mb ma))
         (d^2 (expt d 2))
         (pa (coerce (/ wa w) 'double-float))
         (pb (coerce (/ wb w) 'double-float))
         (m (+ ma (* pb d)))
         (s2 (when (and s2a s2b)
               (+ s2a s2b
                  (* d^2 (/ wab w)))))
         (s3 (when (and s2 s3a s3b)
               (+ s3a s3b
                  (* (expt d 3) (/ (* wab (- wa wb)) (expt w 2)))
                  (* 3 (- (* pa s2b) (* pb s2a)) d))))
         (s4 (when (and s3 s4a s4b)
               (+ s4a s4b
                  (* (expt d 4)
                     (/ (* wab (- (+ (expt wa 2) (expt wb 2)) wab))
                        (expt w 3)))
                  (* 6 d^2
                     (+ (* (expt pa 2) s2b)
                        (* (expt pb 2) s2a)))
                  (* 4 d (- (* pa s3b) (* pb s3a)))))))
    (make-central-sample-moments :w w :m m :s2 s2 :s3 s3 :s4 s4)))

;; (define-structure-== central-sample-moments (n m1 s2 s3 s4))

(defun central-sample-moments-degree (central-sample-moments)
  "Return the degree of CENTRAL-SAMPLE-MOMENTS."
  (let+ (((&structure-r/o central-sample-moments- s2 s3 s4)
          central-sample-moments))
    (cond
      (s4 4)
      (s3 3)
      (s2 2)
      (t 1))))

(defparameter *central-sample-moments-default-degree* 4
  "Default degree for (weighted) central sample moments.")

(defgeneric central-sample-moments (object &key degree weights)
  (:documentation "Return a CENTRAL-SAMPLE-MOMENTS object that allows the
calculation of the central sample moments of OBJECT up to the given DEGREE.

When WEIGHTS are given, they need to be a sequence of matching length.")
  (:method ((object null)
            &key (degree *central-sample-moments-default-degree*)
                 weights)
    (check-type degree (integer 1 4))
    (assert (not weights))
    (make-central-sample-moments :s2 (when (<= 2 degree) 0d0)
                                 :s3 (when (<= 3 degree) 0d0)
                                 :s4 (when (<= 4 degree) 0d0)))
  (:method ((moments central-sample-moments)
            &key (degree (central-sample-moments-degree moments) degree?)
                 weights)
    (assert (or (not degree?) (<= degree (central-sample-moments-degree moments)))
            () 'information-not-collected-in-accumulator)
    (assert (not weights))
    moments)
  (:method ((sequence sequence)
            &key (degree *central-sample-moments-default-degree*)
                 weights)
    (if weights
        (aprog1 (central-sample-moments nil :degree degree)
          (assert (length= sequence weights))
          (map nil (lambda (e w) (add it e :weight w)) sequence weights))
        (aprog1 (central-sample-moments nil :degree degree)
          (map nil (curry #'add it) sequence)))))

(defmacro define-central-sample-moment (function (variable degree) &body body)
  "FIXME documentation, factor our general part"
  (check-type degree (integer 1 4))
  (let+ (((&values remaining-forms declarations docstring)
          (parse-body body :documentation t))
         (body (append declarations remaining-forms)))
    `(defgeneric ,function (object &key weights)
       ,@(splice-awhen docstring `(:documentation ,it))
       (:method (object &key weights)
         (,function (central-sample-moments object :degree ,degree :weights weights)))
       (:method ((,variable central-sample-moments) &key weights)
         (assert (not weights) () "You can't re-weight an accumulator.")
         ,@body))))

(define-central-sample-moment mean (object 1)
  "The mean of elements in OBJECT."
  (central-sample-moments-m object))

(define-central-sample-moment variance (object 2)
  "Variance of OBJECT.  For samples, normalized by the weight-1 (and thus unbiased if certain assumptions hold, eg weights that count frequencies)."
  (let+ (((&structure-r/o central-sample-moments- w s2) object))
    (assert s2 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (assert (< 1 w) () 'not-enough-elements-in-accumulator)
    (/ s2 (1- w))))

(defgeneric sd (object &key weights)
  (:documentation "Standard deviation.  For samples, the square root of the unbiased estimator (see VARIANCE).")
  (:method (object &key weights)
    (sqrt (variance object :weights weights))))

(define-central-sample-moment central-m2 (object 2)
  "Second central moment.  For samples, normalized by the total weight (and thus not the unbiased estimator, see VARIANCE)."
  (let+ (((&structure-r/o central-sample-moments- w s2) object))
    (assert s2 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s2 w)))

(define-central-sample-moment central-m3 (object 3)
  "Third central moment."
  (let+ (((&structure-r/o central-sample-moments- w s3) object))
    (assert s3 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s3 w)))

(define-central-sample-moment central-m4 (object 4)
  "Fourth central moment."
  (let+ (((&structure-r/o central-sample-moments- w s4) object))
    (assert s4 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s4 w)))

(define-central-sample-moment skewness (object 3)
  "Skewness FIXME talk about bias, maybe implement unbiased?"
  (/ (central-m3 object)
     (expt (central-m2 object) 3/2)))

(define-central-sample-moment kurtosis (object 4)
  "Kurtosis FIXME talk about bias, maybe implement unbiased?"
  (/ (central-m4 object)
     (expt (central-m2 object) 2)))

;;; sorted reals, used for implementing quantiles
(defstruct sorted-reals
  "Accumulator which sorts elements.  ELEMENTS return the sorted elements."
  (ordered-elements #() :type vector)
  (unordered-elements nil :type list))

(define-structure-let+ (sorted-reals)
                       ordered-elements unordered-elements)

(defmethod add ((accumulator sorted-reals) object &key)
  (push object (sorted-reals-unordered-elements accumulator)))

(defun sorted-reals-elements (sorted-reals)
  (let+ (((&sorted-reals ordered-elements unordered-elements) sorted-reals))
    (when unordered-elements
      (setf ordered-elements (concatenate 'vector ordered-elements
                                          unordered-elements)
            unordered-elements nil
            ordered-elements (sort ordered-elements #'<)))
    ordered-elements))

(defmethod print-object ((acc sorted-reals) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (acc stream :type t)
        (let+ (((&accessors-r/o (elements sorted-reals-elements)) acc))
          (if (plusp (length elements))
              (format stream "min: ~A, q25: ~A, q50: ~A, q75: ~A, max: ~A"
                      (aref elements 0)
                      (quantile acc 0.25)
                      (quantile acc 0.5)
                      (quantile acc 0.75)
                      (aref elements (1- (length elements))))
              (format stream "no elements"))))))

(defun sort-reals (sequence)
  "Return a SORTED-REALS structure."
  (make-sorted-reals :ordered-elements (sort (copy-sequence 'vector sequence)
                                             #'<)
                     :unordered-elements nil))

(defgeneric ensure-sorted-reals (object)
  (:documentation "Return the contents of OBJECT as a SORTED-REALS.")
  (:method ((sorted-reals sorted-reals))
    sorted-reals)
  (:method ((array array))
    (sort-reals (aops:flatten array)))
  (:method ((list list))
    (sort-reals list)))

(defun ensure-sorted-vector (object)
  "Return the elements of OBJECT as a vector (or reals) sorted in ascending order."
  (sorted-reals-elements (ensure-sorted-reals object)))

(defgeneric quantile (object q)
  (:documentation "Return an element at quantile Q.  May be an interpolation or an approximation, depending on OBJECT and Q.  NOTE: Extensions should define methods for QUANTILES, not QUANTILE.")
  (:method ((object sequence) q)
    (quantile (ensure-sorted-reals object) q ))
  (:method (object q)
    (aref (quantiles object (vector q)) 0)))

(defun empirical-quantile (sorted-vector q)
  "Return the empirical quantile of a vector of real numbers, sorted in ascending order (not checked).  Uses a 0.5 correction."
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
               (lerp frac left (value (1+ int)))))))))

(defun empirical-quantile-probabilities (n)
  "Probabilities that correspond to the empirical quantiles of a vector of length N.  That is to say,

 (== (quantiles sample (empirical-quantile-probabilities (length sample)))
     sample)

for any vector SAMPLE."
  (numseq (/ (* 2 n)) nil :length n :by (/ n) :type 'rational))

(defgeneric quantiles (object qs)
  (:documentation "Multiple quantiles (see QUANTILE).  NOTE: Extensions should define methods for QUANTILES, not QUANTILE.")
  (:method ((object sequence) qs)
    (quantiles (ensure-sorted-reals object) qs))
  (:method ((accumulator sorted-reals) q)
    (map 'vector
         (curry #'empirical-quantile (sorted-reals-elements accumulator)) q)))

(defgeneric median (object)
  (:documentation "Median of OBJECT.")
  (:method ((object sequence))
    (alexandria:median object))
  (:method (object)
    (quantile object 0.5)))

;;; weighted quantiles

(defun weighted-quantile-p-table (weights)
  "Return table of probability brackets for weighted quantile calculations., built from the weights (which should be positive reals, not checked).  Uses a 0.5 correction."
  (aprog1 (aops:make-array-like weights)
    (loop with sum = (sum weights)
          with cumulative-sum = 0
          for index from 0
          for w across weights
          do (setf (aref it index) (/ (+ cumulative-sum (/ w 2)) sum))
             (incf cumulative-sum w))))

(defun weighted-empirical-quantile (sorted-reals p-table q)
  "Return the empirical quantile of a vector of real numbers, sorted in ascending order (not checked).  Uses a 0.5 correction."
  (let+ ((p-index (binary-search p-table q)))
    (cond
      ((or (< q 0) (< 1 q)) (error "Quantile ~A is not in [0,1]." q))
      ((not p-index) (aref sorted-reals 0))
      ((eq p-index t) (aref sorted-reals (1- (length sorted-reals))))
      (t (let+ ((p-left (aref p-table p-index))
                (left (aref sorted-reals p-index)))
           (if (= p-left q)
               left
               (let (
                     (p-right (aref p-table (1+ p-index))))
                 (lerp (/ (- q p-left)
                          (- p-right p-left))
                       left
                       (aref sorted-reals (1+ p-index))))))))))

(defun weighted-quantiles (values weights qs)
  "Calculate quantiles QS of weighted observations.  Uses a 0.5 correction."
  (let* ((pairs (map 'vector #'cons values weights))
         (pairs (sort pairs #'<= :key #'car))
         (sorted-reals (map 'vector #'car pairs))
         (sorted-weights (map 'vector #'cdr pairs))
         (p-table (weighted-quantile-p-table sorted-weights)))
    (map 'vector (lambda (q)
                   (weighted-empirical-quantile sorted-reals p-table q))
         qs)))

;;; sparse counters

(defstruct (sparse-counter (:constructor make-sparse-counter%))
  (table nil :type hash-table :read-only t))

(defun make-sparse-counter (&key (test #'equal))
  "Create a sparse counter.  Elements are compared with TEST (should be accepted by HASH-TABLE)."
  (make-sparse-counter% :table (make-hash-table :test test)))

(defmethod add ((accumulator sparse-counter) object &key (weight 1))
  (assert (non-negative-real-p weight) () "Weight has to be nonnegative.")
  (incf (gethash object (sparse-counter-table accumulator) 0) weight)
  object)

(defmethod tally ((accumulator sparse-counter))
  (let ((sum 0))
    (maphash (lambda (object count)
               (declare (ignore object))
               (incf sum count))
             (sparse-counter-table accumulator))
    sum))

(defmethod as-alist ((object sparse-counter))
  "Return (OBJECT . COUNT) pairs as an alist."
  (hash-table-alist (sparse-counter-table object)))

(defun sparse-counter-count (sparse-counter object)
  "Return the count for OBJECT."
  (gethash object (sparse-counter-table sparse-counter) 0))

(defmethod print-object ((sparse-counter sparse-counter) stream)
  (let+ (((&structure-r/o sparse-counter- table) sparse-counter)
         (varieties (hash-table-count table))
         (alist (sort (as-alist sparse-counter) #'>= :key #'cdr))
         (tally (reduce #'+ alist :key #'cdr))
         ((&values print-length truncated?)
          (cl-num-utils.print-matrix:print-length-truncate varieties)))
    (print-unreadable-object (sparse-counter stream :type t)
      (format stream "tally: ~D, varieties: ~D" tally varieties)
      (loop repeat print-length
            for (object . count) in alist
            do (format stream "~&  ~a  ~d  (~,1f%)" object count
                       (round* (* 100 (/ count tally)) 1/10)))
      (when truncated?
        (format stream "  ~&...")))))

(defun tabulate (sequence &key (test #'equalp))
  "Tabulate a sequence (using a SPARSE-COUNTER with the given TEST)."
  (aprog1 (make-sparse-counter :test test)
    (map nil (curry #'add it) sequence)))

(defun cross-tabulate (sequence1 sequence2 &key (test #'equalp))
  "Cross-tabulate two sequences (using a SPARSE-COUNTER with the given TEST).  TEST is used to compare conses."
  (assert (length= sequence1 sequence2))
  (aprog1 (make-sparse-counter :test test)
    (map nil (lambda (s1 s2)
               (add it (cons s1 s2))) sequence1 sequence2)))

;; ;;; NOTE old code below

;; ;;; Generic interface

;; ;; (defgeneric sweep (accumulator object &key key)
;; ;;   (:documentation "Apply ACCUMULATOR to elements of OBJECT.  When ACCUMULATOR
;; ;;   is a function, it is used to generate a conforming accumulator.")
;; ;;   (:method (accumulator (sequence sequence) &key (key #'identity))
;; ;;     (with-conforming-accumulator (accumulator add)
;; ;;       (map nil (compose #'add key) sequence)))
;; ;;   (:method (accumulator (array array) &key (key #'identity))
;; ;;     (with-conforming-accumulator (accumulator add)
;; ;;       (map nil (compose #'add key) (flatten-array array)))))

;; ;; (defgeneric sample-ratio (object)
;; ;;   (:documentation "Return the proportion of non-nil elements.")
;; ;;   (:method (object)
;; ;;     (let ((accumulator (sweep (sample-ratio-accumulator) object)))
;; ;;       (values (sample-ratio accumulator) accumulator))))


;; ;; ;;; Specific accumulators

;; ;; ;;; tallier


;; ;; (defmethod == ((a tallier) (b tallier) &optional tolerance)
;; ;;   (declare (ignore tolerance))
;; ;;   (= (tallier-tally a) (tallier-tally b)))

;; ;; ;;; sample ratio

;; ;; (defstruct (sample-ratio-accumulator
;; ;;             (:constructor sample-ratio-accumulator ())
;; ;;             (:include tallier))
;; ;;   "Sample ratio accumulator."
;; ;;   (count 0 :type fixnum))

;; ;; (defmethod add ((accumulator sample-ratio-accumulator) object)
;; ;;   (let+ (((&structure sample-ratio-accumulator- tally count) accumulator))
;; ;;     (incf tally)
;; ;;     (when object
;; ;;       (incf count))))

;; ;; (defmethod sample-ratio ((accumulator sample-ratio-accumulator))
;; ;;   (let+ (((&structure-r/o sample-ratio-accumulator- tally count) accumulator))
;; ;;     (/ count tally)))

;; ;; ;;; mean accumulator for scalars

;; ;; ;; (defun pooled-mean (tally1 mean1 tally2 mean2
;; ;; ;;                     &optional (tally (+ tally1 tally2)))
;; ;; ;;   "Pooled mean.  For internal use."
;; ;; ;;   (/ (+ (* tally1 mean1) (* tally2 mean2)) tally))

;; ;; ;; (defmethod pool2 ((acc1 mean-accumulator) (acc2 mean-accumulator))
;; ;; ;;   (let+ (((&structure mean-accumulator- (tally1 tally) (mean1 mean)) acc1)
;; ;; ;;          ((&structure mean-accumulator- (tally2 tally) (mean2 mean)) acc2)
;; ;; ;;          (tally (+ tally1 tally2)))
;; ;; ;;     (mean-accumulator tally (pooled-mean tally1 mean1 tally2 mean2 tally))))

;; ;; ;; (defmethod == ((acc1 mean-accumulator) (acc2 mean-accumulator)
;; ;; ;;                &optional (tolerance *==-tolerance*))
;; ;; ;;   (let+ (((&structure mean-accumulator- (tally1 tally) (mean1 mean)) acc1)
;; ;; ;;          ((&structure mean-accumulator- (tally2 tally) (mean2 mean)) acc2))
;; ;; ;;     (and (= tally1 tally2)
;; ;; ;;          (== mean1 mean2 tolerance))))

;; ;; ;;; mean accumulator for arrays

;; ;; (defstruct (array-mean-accumulator
;; ;;              (:constructor array-mean-accumulator% (mean)))
;; ;;   "Array of accumulators."
;; ;;   (tally 0 :type fixnum)
;; ;;   (mean nil :type array :read-only t))

;; ;; (define-structure-let+ (array-mean-accumulator) tally mean)

;; ;; (defun array-mean-accumulator (dimensions)
;; ;;   "Create an array mean accumulator for array."
;; ;;   (array-mean-accumulator% (make-array dimensions :initial-element 0d0)))

;; ;; (defmethod add ((accumulator array-mean-accumulator) object)
;; ;;   (let+ (((&array-mean-accumulator tally nil) accumulator)
;; ;;          ((&array-mean-accumulator-r/o nil mean) accumulator)
;; ;;          (array (aprog1 (as-array object)
;; ;;                   (assert (common-dimensions it mean))))
;; ;;          (tally (incf tally)))
;; ;;     (dotimes (index (array-total-size array))
;; ;;       (incf-mean (row-major-aref mean index)
;; ;;                  (row-major-aref array index) tally))))

;; ;; (define-structure-slot-accessor mean array-mean-accumulator :read-only? t)

;; ;; (define-conforming-accumulator (mean (array array))
;; ;;   (array-mean-accumulator (array-dimensions array)))

;; ;;; covariance

;; (defstruct sample-covariance
;;   "Sample covariance calculated on-line/single-pass.

;;    The elements are referred to as (X,Y) pairs.

;;    N     count of elements
;;    X-M1  mean of X
;;    X-S2  sum of squared deviations of X from the mean
;;    Y-M1  mean of Y
;;    Y-S2  sum of squared deviations of Y from the mean
;;    XY-S2 sum of the product of X and Y deviations from the mean

;; Allows on-line, numerically stable calculation of moments.  See \cite{bennett2009numerically} and \cite{pebay2008formulas} for the description of the algorithm."
;;   (n 0 :type (integer 0))
;;   (x-m1 0d0 :type double-float)
;;   (x-s2 0d0 :type double-float)
;;   (y-m1 0d0 :type double-float)
;;   (y-s2 0d0 :type double-float)
;;   (xy-s2 0d0 :type double-float))

;; (defmethod add-pair ((accumulator sample-covariance) (x real) (y real))
;;   (let+ ((x (coerce x 'double-float))
;;          (y (coerce y 'double-float))
;;          ((&structure sample-covariance- n x-m1 y-m1 x-s2 y-s2 xy-s2) accumulator)
;;          (x-delta (- x x-m1))
;;          (y-delta (- y y-m1)))
;;     (incf n)
;;     (incf x-m1 (/ x-delta n))
;;     (incf y-m1 (/ y-delta n))
;;     (let ((x-post-delta (- x x-m1))
;;           (y-post-delta (- y y-m1)))
;;       (incf x-s2 (* x-delta x-post-delta))
;;       (incf y-s2 (* y-delta y-post-delta))
;;       (incf xy-s2 (* x-post-delta y-delta)))))

;; (defun covariance (sample-covariance)
;;   "Return the covariance from an accumulator."
;;   (let+ (((&structure-r/o sample-covariance- n xy-s2) sample-covariance))
;;     (when (< 1 n)
;;       (/ xy-s2 (1- n)))))

;; (defun correlation (sample-covariance)
;;   "Return the correlation from an accumulator."
;;   (let+ (((&structure-r/o sample-covariance- n x-s2 y-s2 xy-s2) sample-covariance)
;;          (denominator (sqrt (* x-s2 y-s2))))
;;     (cond
;;       ((plusp denominator) (/ xy-s2 denominator))
;;       ((plusp n) 0)
;;       (t nil))))

;; (defun x-moments (sample-covariance)
;;   "Return a CENTRAL-SAMPLE-MOMENTS object, containing the moments of X."
;;   (let+ (((&structure-r/o sample-covariance- n x-m1 x-s2) sample-covariance))
;;     (make-central-sample-moments :n n :m1 x-m1 :s2 x-s2 :s3 nil :s4 nil)))

;; (defun y-moments (sample-covariance)
;;   "Return a CENTRAL-SAMPLE-MOMENTS object, containing the moments of Y."
;;   (let+ (((&structure-r/o sample-covariance- n y-m1 y-s2) sample-covariance))
;;     (make-central-sample-moments :n n :m1 y-m1 :s2 y-s2 :s3 nil :s4 nil)))

;; (defun sample-covariance (x y)
;;   "Sample covariance accumulator of reals in two sequences."
;;   (assert (length= x y))
;;   (aprog1 (make-sample-covariance)
;;     (map nil (curry #'add-pair it) x y)))

;; (defun covariance-xy (x y)
;;   "Covariance of reals in two sequences."
;;   (covariance (sample-covariance x y)))

;; (defun correlation-xy (x y)
;;   "Correlation of reals in two sequences."
;;   (correlation (sample-covariance x y)))

;; (defmethod == ((acc1 covariance-accumulator) (acc2 covariance-accumulator)
;;                &optional (tolerance *==-tolerance*))
;;   (let+ (((&covariance-accumulator x-mean1 x-sse1 y-mean1 y-sse1
;;                                    cross-sse1) acc1)
;;          ((&covariance-accumulator x-mean2 x-sse2 y-mean2 y-sse2
;;                                    cross-sse2) acc2))
;;     (and (= (tally acc1) (tally acc2))
;;          (== x-mean1 x-mean2 tolerance)
;;          (== x-sse1 x-sse2 tolerance)
;;          (== y-mean1 y-mean2 tolerance)
;;          (== y-sse1 y-sse2 tolerance)
;;          (== cross-sse1 cross-sse2 tolerance))))

;; ;;; autocovariance accumulator

;; (defstruct (autocovariance-accumulator
;;              (:constructor autocovariance-accumulator%))
;;   "Autocovariance accumulator.  Handles missing values (NIL)."
;;   (circular-buffer nil :type list)
;;   (covariance-accumulators nil :type vector))

;; (define-default-add autocovariance-accumulator)

;; (defun autocovariance-accumulator (lags)
;;   "Create an autocovariance accumulator with a given number of lags."
;;   (autocovariance-accumulator%
;;    :circular-buffer (make-circular-list lags :initial-element nil)
;;    :covariance-accumulators (generate-array lags #'covariance-accumulator)))

;; (defgeneric lags (accumulator)
;;   (:documentation "Return the maximum number of available lags in
;;   ACCUMULATOR.")
;;   (:method ((accumulator autocovariance-accumulator))
;;     (length (autocovariance-accumulator-covariance-accumulators
;;              accumulator))))

;; (defmethod add ((accumulator autocovariance-accumulator) (x number))
;;   (let+ (((&structure autocovariance-accumulator- circular-buffer
;;                       covariance-accumulators) accumulator))
;;     ;; update covariances
;;     (iter
;;       (for cov-acc :in-vector covariance-accumulators :downto 0)
;;       (for elt :in circular-buffer)
;;       (when elt
;;          (add cov-acc (cons x elt))))
;;     ;; save element and update circular buffer
;;     (setf (car circular-buffer) x
;;           circular-buffer (cdr circular-buffer))))

;; (defmethod add ((accumulator autocovariance-accumulator) (x null))
;;   ;; just move circular buffer
;;   (let+ (((&structure autocovariance-accumulator- circular-buffer)
;;           accumulator))
;;     ;; save element and update circular buffer
;;     (setf (car circular-buffer) nil
;;           circular-buffer (cdr circular-buffer))))

;; (defgeneric autocovariances (object &optional lags)
;;   (:documentation "Autocovariances.")
;;   (:method ((accumulator autocovariance-accumulator) &optional lags)
;;     (subseq
;;      (map1 #'covariance
;;            (autocovariance-accumulator-covariance-accumulators accumulator))
;;      0 lags))
;;   (:method (object &optional lags)
;;     (let ((accumulator (autocovariance-accumulator lags)))
;;       (values (autocovariances (sweep accumulator object) lags)
;;               accumulator))))

;; (defgeneric autocorrelations (object &optional lags)
;;   (:documentation "Vector of autocorrelations.")
;;   (:method ((accumulator autocovariance-accumulator) &optional lags)
;;     (subseq
;;      (map1 #'correlation
;;            (autocovariance-accumulator-covariance-accumulators accumulator))
;;      0 lags))
;;   (:method (object &optional lags)
;;     (let ((accumulator (autocovariance-accumulator lags)))
;;       (values (autocorrelations (sweep accumulator object) lags)
;;               accumulator))))

;; (defmethod == ((acc1 autocovariance-accumulator)
;;                (acc2 autocovariance-accumulator)
;;                &optional (tolerance *==-tolerance*))
;;   (== (autocovariance-accumulator-covariance-accumulators acc1)
;;       (autocovariance-accumulator-covariance-accumulators acc2)
;;       tolerance))

;; (defmethod print-object ((object autocovariance-accumulator) stream)
;;   (if *print-readably*
;;       (call-next-method)
;;       (print-unreadable-object (object stream :type t)
;;         (format stream "autocorrelations: ~A" (autocorrelations object)))))


;; ;;; sparse accumulator arrays

;; (defstruct (at (:constructor at (object &rest subscripts)))
;;   "Object (observation) with given subscripts."
;;   (object)
;;   (subscripts nil :type list))

;; (defstruct sparse-accumulator-array
;;   "See documentation of the function SPARSE-ACCUMULATOR-ARRAY."
;;   (table (make-hash-table :test #'equal) :type hash-table :read-only t)
;;   (generator nil :type function :read-only t)
;;   (rank nil :type fixnum :read-only t)
;;   (limits nil :type list))

;; (defun sparse-accumulator-array (rank generator)
;;   "Create a sparse accumulator array.  Elements are added with subscripts (see
;; the function AT), and GENERATOR is called to generate an accumulator when the
;; first element is added with given subscripts.  Use LIMITS and REF to access
;; the result."
;;   (make-sparse-accumulator-array :generator generator :rank rank))

;; (defun valid-subscripts? (subscripts rank)
;;   "Check if subscripts are valid (list of fixnums of length RANK)."
;;   (let ((n 0))
;;     (and (every (lambda (x)
;;                   (incf n)
;;                   (typep x 'fixnum))
;;                 subscripts)
;;          (= rank n))))

;; (defun sparse-accumulator-ref% (sparse-accumulator-array subscripts
;;                                 &optional save-new?)
;;   "Return the accumulator corresponding to subscripts, or create one on demand
;; when there isn't one.  When SAVE-NEW?, the latter is saved to the array,
;; otherwise it isn't."
;;   (let+ (((&structure-r/o sparse-accumulator-array- table generator rank)
;;           sparse-accumulator-array)
;;          ((&assert (valid-subscripts? subscripts rank)))
;;          ((&values accumulator present?) (gethash subscripts table)))
;;     (unless present?
;;       (setf accumulator (funcall generator))
;;       (when save-new?
;;         (setf (gethash subscripts table) accumulator)
;;         (let+ (((&structure sparse-accumulator-array- limits)
;;                 sparse-accumulator-array))
;;           (if limits
;;               (map nil (lambda (l s)
;;                          (minf (car l) s)
;;                          (maxf (cdr l) (1+ s)))
;;                    limits subscripts)
;;               (setf limits (mapcar (lambda (s) (cons s s)) subscripts))))))
;;     accumulator))

;; (defmethod add ((instance sparse-accumulator-array) (object at))
;;   (add (sparse-accumulator-ref% instance (at-subscripts object) t)
;;        (at-object object)))



;; ;;; !!! define (add instance (at object subscripts)) compiler macro

;; (defgeneric ref (object &rest subscripts)
;;   (:documentation "Generalization of AREF.")
;;   (:method ((instance sparse-accumulator-array) &rest subscripts)
;;     (sparse-accumulator-ref% instance subscripts)))

;; (defgeneric limits (object)
;;   (:documentation "Return index limits of object.")
;;   (:method ((instance sparse-accumulator-array))
;;     (sparse-accumulator-array-limits instance)))

;; (defmethod as-array ((object sparse-accumulator-array) &key once-only? copy?)
;;   (assert (not copy?) () "Copying is not implemented.")
;;   (let+ (((&structure-r/o sparse-accumulator-array- table generator limits)
;;           object)
;;          (offset (mapcar #'car limits))
;;          (dimensions (mapcar (lambda (c) (- (cdr c) (car c))) limits))
;;          (array (generate-array dimensions
;;                                 (if once-only?
;;                                     (funcall generator)
;;                                     generator))))
;;     (maphash (lambda (key value)
;;                (setf (apply #'aref array (mapcar #'- key offset)) value))
;;              table)
;;     (values array offset)))

;; (defmethod keys-and-values ((object sparse-accumulator-array))
;;   (keys-and-values (sparse-accumulator-array-table object)))

;; ;;; moments accumulator

;; ;; (defclass moments-accumulator-array (sparse-accumulator-array )
;; ;;   ()
;; ;;   (:documentation ""))

;; ;; (defun moments-accumulator-array (rank &key (accumulator #'mean-accumulator))
;; ;;   (make-instance 'moments-accumulator-array :rank rank
;; ;;                  :init-function (lambda (&rest rest)
;; ;;                                   (declare (ignore rest))
;; ;;                                   (funcall accumulator))))

;; ;;; acf accumulator

;; ;; (defstruct (residual-pair (:constructor residual-pair (x x-index y y-index)))
;; ;;   "Pair of residuals."
;; ;;   (x)
;; ;;   (x-index nil :type fixnum)
;; ;;   (y)
;; ;;   (y-index nil :type fixnum))

;; ;; (defclass acf-accumulator (sparse-accumulator-array)
;; ;;   ()
;; ;;   (:documentation ""))

;; ;; (defmethod add ((instance acf-accumulator) (residual-pair residual-pair))
;; ;;   ;; !!! factor out part, write compiler macro
;; ;;   (let+ (((&structure residual-pair- x x-index y y-index) residual-pair))
;; ;;     (when (<= x-index y-index)
;; ;;       (add-with-subscripts% instance (* x y) (list (- y-index x-index))))))


;; ;;; Histograms
;; ;;;
;; ;;; data structures for counting the frequencies of multivariate indexes
;; ;;; (always of the same rank).  Building block for a histogram, but does not
;; ;;; have information on where the indexes came from.  Frequencies can only be
;; ;;; added, not set or subtracted.  The total is available, but is not
;; ;;; necessarily cached/saved.  Some combinations of subscripts may be invalid
;; ;;; or unavailable, in that case an error is raised.
;; ;;;
;; ;;; !! define condition
;; ;;; !! write methods for using an array as frequencies

;; (defstruct (histogram-accumulator
;;             (:include sparse-accumulator-array))
;;   bins)

;; (defmethod bins ((accumulator histogram-accumulator))
;;   (histogram-accumulator-bins accumulator))

;; (defun histogram-accumulator (&rest bins)
;;   (make-histogram-accumulator :bins bins :generator #'tallier
;;                               :rank (length bins)))

;; (defmethod add ((accumulator histogram-accumulator) (sequence sequence))
;;   (let+ (((&structure-r/o histogram-accumulator- bins) accumulator))
;;     (assert (= (length sequence) (length bins)))
;;     (add accumulator (apply #'at 1 (map 'list #'bin-index bins sequence)))))

;; (defmethod add ((accumulator histogram-accumulator) (number number))
;;   (add accumulator (list number)))

;; (defmethod keys-and-values ((accumulator histogram-accumulator))
;;   (map 'vector (lambda+ ((k . v))
;;                  (cons k (tally v)))
;;        (call-next-method)))

;; (defun locations-and-tallies (histogram-accumulator)
;;   (let+ (((&structure-r/o histogram-accumulator- bins) histogram-accumulator))
;;     (map 'vector (lambda+ ((k . v))
;;                    (cons (mapcar (lambda (b i) (bin-location b i))
;;                                  bins k)
;;                          v))
;;          (keys-and-values histogram-accumulator))))

;; (defun location-limits (histogram-accumulator)
;;   (mapcar (lambda (l b)
;;             (interval-hull (list (bin-location b (car l))
;;                                  (bin-location b (cdr l)))))
;;           (limits histogram-accumulator)
;;           (bins histogram-accumulator)))

;; (defmethod ref ((accumulator histogram-accumulator) &rest subscripts)
;;   (tally (apply #'call-next-method accumulator subscripts)))

;; (defparameter *frequency-print-width* 40
;;   "The number of columns used for printing frequencies using text symbols.
;;   Does not include the space used by labels etc.")

;; (defun print-univariate-frequencies% (stream labels-frequencies)
;;   "Print univaritate frequencies from TABLE to stream.  LABELS-FREQUENCIES
;; should be a vector of sorted (LABEL . FREQUENCY) conses.  Not exported."
;;   (iter
;;     (for (label . frequency) :in labels-frequencies)
;;     (maximize (length label) :into label-width)
;;     (maximize frequency :into maximum-frequency)
;;     (finally
;;      (let ((step (max (pretty-step maximum-frequency *frequency-print-width*)
;;                       1)))
;;        (loop for (label . frequency) in labels-frequencies
;;              do (format stream "~&~vA " label-width label)
;;                 (let+ (((&values quotient remainder) (floor frequency step)))
;;                   (loop repeat quotient do (princ #\* stream))
;;                   (when (plusp remainder) (princ #\. stream))))
;;        (format stream "~&(each * = frequency of ~A)" step)))))

;; (defmethod print-object ((accumulator histogram-accumulator) stream)
;;   (let+ (((&accessors-r/o limits) accumulator))
;;     (if (= (length limits) 1)
;;         (let+ ((((start . end)) limits)
;;                ((bin) (bins accumulator))
;;                (labels-frequencies
;;                 (iter
;;                   (for index :from start :below end)
;;                   (collect (cons (format-bin-location (bin-location bin index))
;;                                  (ref accumulator index))))))
;;           (print-unreadable-object (accumulator stream :type t)
;;             (print-univariate-frequencies% stream labels-frequencies)))
;;         (call-next-method))))

;; (defun scott-rule (object &key (pretty? t) (correction 1d0) offset)
;;   "Return a bin object using Scott's (1979) rule for bin width, multiplied by
;; CORRECTION, rounded when PRETTY?.  When OFFSET is given, it is used for the
;; bins, otherwise they are centered around 0."
;;   (let+ (((&accessors-r/o tally sd) (sweep 'sse object))
;;          (width (* 3.5d0 (expt tally -1/3) sd correction))
;;          (width (if pretty?
;;                     (pretty width)
;;                     width)))
;;     (even-bins width (aif offset it (/ width 2)))))

;; (defun histogram1 (object &optional (bins #'scott-rule))
;;   "Return a univariate histogram of OBJECT.  If BINS is a function, call it on
;; object to generate the bins."
;;   (sweep (histogram-accumulator (if (functionp bins)
;;                                     (funcall bins object)
;;                                     bins))
;;          object))

;; ;; (defun histogram-from-matrix (matrix &rest bins)
;; ;;   (let+ ((histogram (apply #'make-hashed-histogram bins))
;; ;;          ((nrow ncol) (array-dimensions matrix)))
;; ;;     (assert (= (subscript-rank histogram) ncol))
;; ;;     (loop for row :below nrow do
;; ;;       (apply #'add-observation histogram
;; ;;              1 (coerce (subarray matrix row) 'list)))
;; ;;     histogram))

;; ;; (defclass binned-data ()
;; ;;   ((indexes :accessor indexes :initarg :indexes)))

;; ;; (defmethod indexes ((vector vector))
;; ;;   vector)

;; ;; (defmethod as-array ((binned-data binned-data) &key copy?)
;; ;;   (maybe-copy-array (indexes binned-data) copy?))

;; ;; (defgeneric bin-limit (binned-data)
;; ;;   (:documentation "Return an integer which larger than all indexes (but does not
;; ;;   have to be the smallest of such values).")
;; ;;   (:method ((vector vector))
;; ;;     (1+ (reduce #'max vector)))
;; ;;   (:method ((binned-data binned-data))
;; ;;     (bin-limit (indexes binned-data))))

;; ;; (defgeneric bin-origin (binned-data bin-index)
;; ;;   (:documentation "Return information on the particular bin (what value/range is
;; ;;   mapped to this bin) if available.")
;; ;;   (:method ((vector vector) bin-index)
;; ;;     bin-index)
;; ;;   (:method ((binned-data binned-data) bin-index)
;; ;;     bin-index))

;; ;; (defgeneric bin-origins (binned-data)
;; ;;   (:documentation "Bin origin for all bins.")
;; ;;   (:method (binned-data)
;; ;;     (iter
;; ;;       (for bin-index :below (bin-limit binned-data))
;; ;;       (collect (bin-origin binned-data bin-index) :result-type vector))))

;; ;; ;;; continuous bins

;; ;; (defclass continuous-binned-data (binned-data)
;; ;;   ((breaks :accessor breaks :initarg :breaks))
;; ;;   (:documentation "Used for binning real numbers."))

;; ;; (defmethod bin-limit ((binned-data continuous-binned-data))
;; ;;   (length (breaks binned-data)))

;; ;; (defmethod bin-origin ((binned-data continuous-binned-data) bin-index)
;; ;;   (bind (((:slots-r/o breaks) binned-data))
;; ;;     (make-interval (aref breaks bin-index)
;; ;;                    (let ((right-index (1+ bin-index)))
;; ;;                      (when (< right-index (length breaks))
;; ;;                        (aref breaks right-index))))))

;; ;; (defun bin-using-breaks (vector breaks &key (below 0)
;; ;;                          (above (- (length breaks) 2)) copy? skip-check?)
;; ;;   (make-instance 'continuous-binned-data
;; ;;                  :indexes (map 'simple-fixnum-vector
;; ;;                                (irregular-bins breaks :below below :above above
;; ;;                                                :copy? copy?
;; ;;                                                :skip-check? skip-check?)
;; ;;                                vector)
;; ;;                  :breaks breaks))

;; ;; (defun bin-using-quantiles (vector quantiles)
;; ;;   "Bin VECTOR using its quantiles.  Quantiles has to contain 0 and 1.  Highest
;; ;; element is put in the last bin."
;; ;;   (assert (and (vector-satisfies? quantiles #'<)
;; ;;                (= (aref quantiles 0) 0)
;; ;;                (= (vector-last quantiles) 1)))
;; ;;   (bin-using-breaks vector (sample-quantiles vector quantiles)
;; ;;                     :skip-check? t))

;; ;; ;;; discrete bins

;; ;; (defclass discrete-binned-data (binned-data)
;; ;;   ((keys :accessor keys :initarg :keys)))

;; ;; (defmethod bin-limit ((binned-data discrete-binned-data))
;; ;;   (length (keys binned-data)))

;; ;; (defmethod bin-origin ((binned-data discrete-binned-data) bin-index)
;; ;;   (aref (keys binned-data) bin-index))

;; ;; (defmethod bin-origins ((binned-data discrete-binned-data))
;; ;;   (keys binned-data))

;; ;; (defun bin-discrete (vector &key (test #'eql))
;; ;;   "Bin discrete data, using TEST.  The implementation uses a hash-table, and
;; ;; TEST has to be acceptable to MAKE-HASH-TABLE."
;; ;;   (let ((table (make-hash-table :test test)))
;; ;;     (map nil (lambda (v)
;; ;;                (setf (gethash v table) t))
;; ;;          vector)
;; ;;     (let ((keys (sort (coerce (hash-table-keys table) 'vector) #'<)))
;; ;;       (iter
;; ;;         (for key :in-vector keys :with-index key-index)
;; ;;         (setf (gethash key table) key-index))
;; ;;       (make-instance 'discrete-binned-datann==
;; ;;                      :indexes (map 'vector (lambda (v) (gethash v table)) vector)
;; ;;                      :keys keys))))


;; ;; (defun weighted-mean-accumulator ()
;; ;;   "Accumulator for online calculation of the weighted mean of observations.  Called
;; ;; with X (a real number) for accumulating the mean.  When called with no
;; ;; arguments, return (values MEAN SW N), MEAN is double-float, SW (sum of weights) is
;; ;; double-float, N is fixnum."
;; ;;   ;; West (1979)
;; ;;   (let ((n 0)
;; ;;         (mean 0)
;; ;;         (sw 0))
;; ;;     (lambda (&optional x w)
;; ;;       (if x
;; ;;           (progn
;; ;;             (incf n)
;; ;;             (incf sw w)
;; ;;             (incf mean (/ (* (- x mean) w) sw)))
;; ;;           (values mean sw n)))))


;; ;; (defun weighted-sse-accumulator ()
;; ;;   "Accumulator for weighted sum of squared errors.  When called without arguments,
;; ;; return (values SSE SW MEAN N), where SW is the sum of weights."
;; ;;   ;; West (1979)
;; ;;   (declare (optimize speed))
;; ;;   (let ((n 0)
;; ;;         (mean 0d0)
;; ;;         (sse 0d0)
;; ;;         (sw 0d0))
;; ;;     (declare (fixnum n))
;; ;;     (lambda (&optional x w)
;; ;;       (if x
;; ;;           (let ((previous-sw sw))
;; ;;             (incf n)
;; ;;             (incf sw w)
;; ;;             (let* ((q (- x mean))
;; ;;                    (r (/ (* q w) sw)))
;; ;;               (incf sse (* previous-sw q r))
;; ;;               (incf mean r)))
;; ;;           (values sse sw mean n)))))

;; ;; (defgeneric weighted-mean (object weights)
;; ;;   (:documentation "Return the weighted sample mean.")
;; ;;   (:method ((sequence sequence) (weights sequence))
;; ;;     (apply-accumulator (weighted-mean-accumulator) sequence weights)))

;; ;; (defgeneric weighted-variance (object weights)
;; ;;   (:documentation "Return the weighted sample mean.  If there are second and third
;; ;;   values, they are the mean and the sum of weights.")
;; ;;   (:method ((sequence sequence) (weights sequence))
;; ;;     (bind (((:values sse sw mean)
;; ;;             (apply-accumulator (weighted-sse-accumulator) sequence weights)))
;; ;;       (values (/ sse (1- sw)) mean))))

;; ;;; !!! todo: mean and variance for matrices
;; ;;;           covariance (by stacking vectors?)
;; ;;;           correlation (matrix)

;; ;; (defun sample-cov (a b &key (a-mean (mean a)) (b-mean (mean b)))
;; ;;   "Sample covariance between A and B.  Means will be used when provided."
;; ;;   (let* ((a (coerce a 'vector))
;; ;;          (b (coerce b 'vector))
;; ;;          (size (length a))
;; ;;          (sum 0))
;; ;;     (assert (= (length b) size) () "Vectors do not have the same length.")
;; ;;     (dotimes (i size)
;; ;;       (incf sum (* (- (aref a i) a-mean)
;; ;;                    (- (aref b i) b-mean))))
;; ;;     (sample-second-moment% sum size)))

;; ;; (defun sample-corr (a b &key (a-mean (mean a)) (b-mean (mean b)))
;; ;;   "Sample correlation between A and B.  Means will be used when provided."
;; ;;   (/ (sample-cov a b :a-mean a-mean :b-mean b-mean)
;; ;;      (sample-sd a a-mean) (sample-sd b b-mean)))

;; ;;; sensible behavior for sequences and arrays

;; ;; (defmethod size ((sequence sequence))
;; ;;   (length sequence))

;; ;; (defmethod size ((array array))
;; ;;   (array-total-size array))

;; ;; (defmethod sum ((sequence sequence))
;; ;;   (reduce #'+ sequence))

;; ;; (defmethod sum ((array array))
;; ;;   (iter
;; ;;     (for index :from 0 :below (array-total-size array))
;; ;;     (summing (row-major-aref array index))))

;; ;; (defmethod sse ((sequence sequence) &optional (mean (mean sequence)))
;; ;;   (reduce #'+ sequence :key (lambda (x) (expt (- x mean) 2))))

;; ;; (defmethod sse ((array array) &optional (mean (mean array)))
;; ;;   (iter
;; ;;     (for index :from 0 :below (array-total-size array))
;; ;;     (summing (expt (- (row-major-aref array index) mean) 2))))

;; (defun subranges (ranges &key shadow-ranges)
;;   "Given a sequence of integer ranges with elements (start . end),
;; return (values SUBRANGES INDEXES-LISTS).  SUBRANGES is a vector of subranges
;; with elements (start . end), and INDEX-LISTS is a vector of lists, enumerating
;; the subintervals that make up the corresponding original interval.  All
;; returned ranges and indexes are in increasing order, but RANGES doesn't have
;; to be.

;; Example:

;;   (subranges #((0 . 100) (50 . 150)))

;;   evaluates to values

;;     #((0 . 50) (50 . 100) (100 . 150))
;;     #((0 1) (1 2))

;; The algorithm makes sure that only subranges which are in a nonempty range are
;; kept.  For example,

;;   (subranges #((0 . 50) (100 . 150)))

;;   evaluates to values

;;     #((0 . 50) (100 . 150))
;;     #((0) (1))

;; When (>= start end), a range is considered empty.  If all ranges are empty,
;; SUBRANGES is #() and INDEX-LISTS contains NILs.  For example,

;;   (subranges #((5 . 0) (6 . 6)))

;;   evaluates to values

;;     #()
;;     #(NIL NIL)

;; Shadow-ranges are ranges which are taken into account when calculating the
;; subranges, but have no corresponding elements in the returned vectors.  For
;; example, :shadow-ranges '(0 . 1000) will make subranges superset of a
;; partition of [0 ... 999]."
;;   (let+ ((n (length shadow-ranges))
;;          (ranges (concatenate 'vector shadow-ranges ranges))
;;          ((&values subranges index-lists) (subranges% ranges)))
;;     (values subranges (subseq index-lists n))))

;; (defun subranges% (ranges)
;;   "Internal function used by subranges."
;;   (let* ((endpoints (coerce (remove-duplicates
;;                              (iter
;;                                (for (start . end) :in-vector ranges)
;;                                (when (< start end)
;;                                  (collect start)
;;                                  (collect end))))
;;                             'simple-fixnum-vector)))
;;     ;; if all ranges are empty, there are no subranges
;;     (unless (plusp (length endpoints))
;;       (return-from subranges% (values #()
;;                                       (make-array (length ranges)
;;                                                   :initial-element nil))))
;;     ;; sort endpoints
;;     (let* ((endpoints (sort endpoints #'<=))
;;            (within-lists (make-array (1- (length endpoints))
;;                                      :initial-element nil)))
;;       ;; check which subintervals are within any range
;;       (iter
;;         (for (start . end) :in-vector ranges :with-index range-index)
;;         (when (< start end)
;;           (iter
;;             (for endpoint :from (binary-search endpoints start)
;;                           :below (binary-search endpoints end))
;;             (push range-index (aref within-lists endpoint)))))
;;       ;; remove those that are not in use
;;       (iter
;;         (for end :in-vector endpoints :from 1)
;;         (for start :previous end :initially (first* endpoints))
;;         (for within-list :in-vector within-lists)
;;         (when within-list
;;           (collect (cons start end) :into subranges :result-type vector)
;;           (collect (nreverse within-list) :into within-lists2))
;;         (finally
;;          ;; for each range, list constituent subranges
;;          (let ((subrange-lists (make-array (length ranges)
;;                                            :initial-element nil)))
;;            (iter
;;              (for within-list :in within-lists2)
;;              (for subrange-index :from 0)
;;              (dolist (within-index within-list)
;;                (push subrange-index (aref subrange-lists within-index))))
;;            (return (values subranges
;;                            (map 'vector #'nreverse subrange-lists)))))))))

;; (defun demean (object &optional (mean (mean object)))
;;   "Subtract mean from object.  The mean is returned as the second value."
;;   (values (e- object mean) mean))

;; (defun aggregate (sequence n function)
;;   "Aggregate groups of N consecutive elements of SEQUENCE (vector preferred)
;; using FUNCTION.

;; Example: (aggregate #(1 2 3 5 7 13) 2 #'mean) => #(1.5d0 4.0d0 10.0d0)."
;;   (map 'vector function
;;        (subarrays 1 (reshape (list t n) (coerce sequence 'vector)))))

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
