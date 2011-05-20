;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Generic interface

(defgeneric add (accumulator object)
  (:documentation "Add accumulator to object."))

(defgeneric apply-accumulator (object accumulator)
  (:documentation "Apply accumulator to object elementwise.")
  (:method ((sequence sequence) accumulator)
    (progn
      (map nil (curry #'add accumulator) sequence)
      accumulator))
  (:method ((array array) accumulator)
    (progn
      (map nil (curry #'add accumulator) (flatten-array array))
      accumulator)))

(defgeneric representative-element (object)
  (:documentation "Return an element that will be used for creating an
  accumulator.  Typically (but not necessarily) the first element if there is
  a traversal order.")
  (:method ((array array))
    (row-major-aref array 0))
  (:method ((list list))
    (car list)))

;;; Predefined accumulators

(defgeneric tally (object)
  (:documentation "Return the count of elements.")
  (:method (object)
    (tally (apply-accumulator object (tallier))))
  (:method ((sequence sequence))
    (length sequence))
  (:method ((array array))
    (array-total-size array)))

(defgeneric conforming-mean-accumulator (element)
  (:documentation "Return an accumulator for the mean, dispatching on an
element of the sequence (typically the first)."))

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method (object)
    (mean (apply-accumulator object 
                             (conforming-mean-accumulator
                              (representative-element object))))))

(defgeneric sse (object &optional center)
  (:documentation "Return the sum of squared errors (from the given CENTER,
  which defaults to the MEAN.  Return NIL when there are no elements.")
  (:method (object &optional center)
    (sse (apply-accumulator object (mean-sse-accumulator))
         center)))

(defgeneric variance (object)
  (:documentation "Return the variance.")
  (:method (object)
    (let* ((accumulator (apply-accumulator object (mean-sse-accumulator)))
           (n-1 (1- (tally accumulator))))
      (when (plusp n-1)
        (/ (sse accumulator) n-1)))))

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
  or an approximation, depending on OBJECT and Q."))

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

(defmethod conforming-mean-accumulator ((number number))
  (mean-accumulator))

;;; mean accumulator for arrays

(defstruct (array-mean-accumulator
             (:include mean-accumulator)
             (:constructor array-mean-accumulator
              (dimensions &optional 
               (size (product dimensions))
               (mean (make-array dimensions :initial-element 0d0)))))
  "Array mean accumulator."
  (dimensions nil :type list)
  (size nil :type fixnum))

(defmethod add ((instance array-mean-accumulator) array)
  (bind (((:structure/rw array-mean-accumulator- tally mean dimensions size)
          instance)
         (array (as-array array)))
    (assert (equal (array-dimensions array) dimensions))
    (incf tally)
    (dotimes (index size)
      (incf (row-major-aref mean index)
            (/ (- (row-major-aref array index)
                  (row-major-aref mean index))
               tally)))))

(defmethod conforming-mean-accumulator ((array array))
  (array-mean-accumulator (array-dimensions array) (array-total-size array)))

;;; mean-sse accumulator

(defstruct (mean-sse-accumulator
             (:constructor mean-sse-accumulator ())
             (:include mean-accumulator))
  ""
  (sse 0d0))

(defmethod add ((instance mean-sse-accumulator) (object number))
  ;; (declare (optimize speed))
  (bind (((:structure/rw mean-sse-accumulator- tally mean sse) instance)
         (previous-mean mean))
    (incf tally)
    (incf mean (/ (- object mean) tally))
    (incf sse (* (- object mean) (- object previous-mean)))))

(defmethod sse ((instance mean-sse-accumulator) &optional center)
  (assert (not center) () "need to implement...")
  (mean-sse-accumulator-sse instance))

;;; storage accumulator

(defstruct (storage-accumulator (:constructor storage-elements ()))
  "Storage accumulator, simply save elements."
  (elements nil :type list))

(defmethod tally ((instance storage-accumulator))
  (length (storage-accumulator-elements instance)))

(defmethod mean ((instance storage-accumulator))
  (mean (storage-accumulator-elements instance)))

(defmethod sse ((instance storage-accumulator) &optional center)
  (sse (storage-accumulator-elements instance) center))

;;; sorted vector -- this is not an accumulator, merely a wrapper indicating
;;; that the vector is sorted.

(defstruct (sorted-vector
             (:constructor sorted-vector (elements &optional (predicate #'<))))
  "Wrapper indicating that elements is sorted."
  (predicate)
  (elements nil :type vector))

(defun sort-as-vector (sequence predicate &key destructive?)
  "Return SEQUENCE sorted as a vector.  When DESTRUCTIVE?, sequence may be
destructively modified."
  (sorted-vector
   (if (vectorp sequence)
       (if destructive?
           (sort sequence predicate)
           (sort (copy-seq sequence) predicate))
       (sort (coerce sequence 'vector) predicate))
   predicate))

(defmethod quantile ((sorted-vector sorted-vector) q)
  (bind (((:structure sorted-vector- predicate elements) sorted-vector))
    (if (eq predicate #'<)
        (bind ((n (length elements))
               (r (* q (1- n)))
               ((:values int frac) (floor r))
               (left (aref elements int)))
          (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
          (if (zerop frac)
              left
              (convex-combination left (aref elements (1+ int)) frac)))
        (sort-as-vector elements #'<))))

(defmethod quantile ((sequence sequence) q)
  (quantile (sort-as-vector sequence #'<) q))

;;; accumulator arrays

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

