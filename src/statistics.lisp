;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Generic interface

(defgeneric add (accumulator object)
  (:documentation "Add accumulator to object."))

(defgeneric statistic (function object)
  (:argument-precedence-order object function)
  (:documentation "Return the desired statistic from object.  Note: this
  function is called for extracting statistics from accumulators which may not
  have a method defined for function, but can return the statistic some other
  way (eg mapping function elementwise).  The default method returns NIL, for
  objects which are not accumulators.  Custom accumulators should define
  methods for this (eg see array-accumulator).")
  (:method (function object)
    nil))

(defun sweep-on-demand (function object generator)
  "When STATISTIC returns NIL, sweep the object, initializing with GENERATOR."
  (aif (statistic function object)
       it
       (funcall function (sweep generator object))))

(defgeneric conforming-accumulator (generator element)
  (:documentation "Return a conforming accumulator using GENERATOR, based on
  the structure of ELEMENT.")
  (:method ((generator function) (element number))
    (funcall generator)))

(defgeneric sweep (accumulator object)
  (:documentation "Apply ACCUMULATOR to elements of OBJECT.  When ACCUMULATOR
  is a function, it is used to generate a conforming accumulator.")
  (:method (accumulator (sequence sequence))
    (map nil (curry #'add accumulator) sequence)
    accumulator)
  (:method ((generator function) (sequence sequence))
    (sweep (conforming-accumulator generator (elt sequence 0)) sequence))
  (:method (accumulator (array array))
    (map nil (curry #'add accumulator) (flatten-array array))
    accumulator)
  (:method ((generator function) (array array))
    (sweep (conforming-accumulator generator (first* array)) array)))

;;; sweep also resolves some symbols and functions into default accumulators,
;;; the helper macros below take care of defining these

(defmacro define-sweep-default-generator (synonym generator)
  "Define a synonym for sweep."
  `(defmethod sweep ((generator (eql ,synonym)) object)
     (sweep ,generator object)))

(defmacro define-sweep-default-generators (synonyms generator)
  "Define multiple synonyms for sweep."
  `(progn
     ,@(loop for synonym in synonyms
             collect `(define-sweep-default-generator ,synonym ,generator))))

;;; accumulator arrays

(defstruct+ (accumulator-array
             (:constructor accumulator-array (accumulators)))
    "Array of accumulators."
  (accumulators nil :type array :read-only t))

(defmethod statistic (function (accumulator accumulator-array))
  (emap t function (accumulator-array-accumulators accumulator)))

(defmethod conforming-accumulator ((generator function) (array array))
  (accumulator-array (filled-array (array-dimensions array) generator)))

(defun add-array-elementwise (accumulator array)
  "Add array to array accumulator elementwise."
  (let+ (((&accumulator-array accumulators) accumulator))
    (assert (common-dimensions array accumulators))
    (dotimes (index (array-total-size array))
      (add (row-major-aref accumulators index)
           (row-major-aref array index)))))

(defmethod add ((accumulator accumulator-array) object)
  (add-array-elementwise accumulator (as-array object)))

;;; Predefined accumulators

(defgeneric tally (object)
  (:documentation "Return the count of elements.")
  (:method (object)
    (sweep-on-demand #'tally object #'tallier))
  (:method ((sequence sequence))
    (length sequence))
  (:method ((array array))
    (array-total-size array)))

(define-sweep-default-generators ('tally #'tally) #'tallier)

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method (object)
    (sweep-on-demand #'mean object #'mean-accumulator)))

(define-sweep-default-generators ('mean #'mean) #'mean-accumulator)

(defgeneric sse (object &optional center)
  (:documentation "Return the sum of squared errors (from the given CENTER,
  which defaults to the MEAN.  Return NIL when there are no elements.")
  (:method (object &optional center)
    (sweep-on-demand (lambda (accumulator)
                       (sse accumulator center))
                     object
                     #'mean-sse-accumulator)))

(defgeneric variance (object)
  (:documentation "Return the variance.")
  (:method (object)
    (sweep-on-demand (lambda (accumulator)
                       (let ((n-1 (1- (tally accumulator))))
                         (when (plusp n-1)
                           (/ (sse accumulator) n-1))))
                     object
                     #'mean-sse-accumulator)))

(define-sweep-default-generators
    ('sse 'variance 'mean-and-variance #'sse #'variance)
    #'mean-sse-accumulator)

(defgeneric sum (object)
  (:documentation "Sum of elements in object.")
  ;; !!! TODO: also with accumulators
  (:method ((sequence sequence))
    (reduce #'+ sequence))
  (:method ((array array))
    (reduce #'+ (flatten-array array))))

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
    (sweep-on-demand (lambda (accumulator)
                       (quantile accumulator q))
                     object
                     #'sorting-accumulator)))

;; (defun median (object)
;;   "Median of OBJECT."
;;   (quantile object 0.5))

;;; Specific accumulators

;;; tallier

(defstruct (tallier (:constructor tallier ()))
  (tally 0 :type fixnum))

(defmethod tally ((instance tallier))
  (tallier-tally instance))

;;; mean accumulator for scalars

(defstruct (mean-accumulator
             (:include tallier)
             (:constructor mean-accumulator ()))
  "Accumulator for a (scalar) mean."
  (mean 0d0))

(defmethod add ((instance mean-accumulator) (object number))
  (bind (((:structure/rw mean-accumulator- tally mean) instance))
    (incf tally)
    (incf mean (/ (- object mean) tally))))

(defmethod mean ((instance mean-accumulator))
  (mean-accumulator-mean instance))

;;; mean accumulator for arrays

;; (defstruct (array-mean-accumulator
;;              (:include mean-accumulator)
;;              (:constructor array-mean-accumulator
;;               (dimensions &optional 
;;                (size (product dimensions))
;;                (mean (make-array dimensions :initial-element 0d0)))))
;;   "Array mean accumulator."
;;   (dimensions nil :type list)
;;   (size nil :type fixnum))

;; (defmethod add ((instance array-mean-accumulator) array)
;;   (bind (((:structure/rw array-mean-accumulator- tally mean dimensions size)
;;           instance)
;;          (array (as-array array)))
;;     (assert (equal (array-dimensions array) dimensions))
;;     (incf tally)
;;     (dotimes (index size)
;;       (incf (row-major-aref mean index)
;;             (/ (- (row-major-aref array index)
;;                   (row-major-aref mean index))
;;                tally)))))

;;; mean-sse accumulator

(defstruct (mean-sse-accumulator
             (:constructor mean-sse-accumulator ())
             (:include mean-accumulator))
  "Mean and sum of squared error accumulator."
  (sse 0d0))

(defmethod add ((instance mean-sse-accumulator) (object number))
  (bind (((:structure/rw mean-sse-accumulator- tally mean sse) instance)
         (previous-mean mean))
    (incf tally)
    (incf mean (/ (- object mean) tally))
    (incf sse (* (- object mean) (- object previous-mean)))))

(defmethod sse ((instance mean-sse-accumulator) &optional center)
  (assert (not center) () "need to implement...")
  (mean-sse-accumulator-sse instance))

;;; sorting accumulator
;;; 
;;; This is not the most elegant way of calculating quantiles, but it will do
;;; until I implement something nicer.

(defstruct+ (sorting-accumulator
             (:constructor sorting-accumulator (&optional (predicate #'<)))
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
    (assert (eq (sorting-accumulator-predicate sorting-accumulator) #'<=) ()
            "Accumulator has to be sorted by <=.")
    (let+ ((n (length elements))
           (r (* q (1- n)))
           ((&values int frac) (floor r))
           (left (aref elements int)))
      (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
      (if (zerop frac)
          left
          (convex-combination left (aref elements (1+ int)) frac)))))

(defmethod quantile (object q)
  (quantile (sweep #'sorting-accumulator object) q))

(define-sweep-default-generators ('quantile #'quantile)
    #'sorting-accumulator)

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

