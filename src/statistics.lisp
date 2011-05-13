;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;; (define-condition not-enough-elements (error)
;;   ())

;;; primitives, also useful for for building defaults

;; (defgeneric size (object)
;;   (:documentation "Number of elements in object."))


;;; mean and variance, can be specialized, but have sensible default
;;; definitions

;;; Accumulators
;;; 

(defun apply-accumulator (accumulator &rest sequences)
  "Accumulators are are closures that can be used to calculate summary statistics
online.  When called with no arguments, they return the desired summary statistics as
values.  This is a convenience function for that purpose."
  (apply #'map nil accumulator sequences)
  (funcall accumulator))

(defun mean-accumulator ()
  "Accumulator for online calculation of the mean of observations.  Called with
X (a real number) for accumulating the mean.  When called with no
arguments, return (values MEAN N), MEAN is double-float, N is fixnum."
  ;; uses Welford's online algorithm
  (declare (optimize speed (safety 0)))
  (let ((n 0)
        (mean 0d0))
    (declare (fixnum n)
             (double-float mean))
    (lambda (&optional x)
      (if x
          (progn
            (incf n)
            (incf mean (/ (- (coerce x 'double-float) mean) n)))
          (values mean n)))))

(defun weighted-mean-accumulator ()
  "Accumulator for online calculation of the weighted mean of observations.  Called
with X (a real number) for accumulating the mean.  When called with no
arguments, return (values MEAN SW N), MEAN is double-float, SW (sum of weights) is
double-float, N is fixnum."
  ;; West (1979)
  (let ((n 0)
        (mean 0)
        (sw 0))
    (lambda (&optional x w)
      (if x
          (let ((x (coerce x 'double-float))
                (w (coerce w 'double-float)))
            (incf n)
            (incf sw w)
            (incf mean (/ (* (- x mean) w) sw)))
          (values mean sw n)))))

(defun sse-accumulator ()
  "Accumulator for sum of squared errors.  When called without arguments,
return (values SSE MEAN N)."
  ;; Welford's online algorithm
  (declare (optimize speed))
  (let ((n 0)
        (mean 0d0)
        (sse 0d0))
    (declare (fixnum n)
             (double-float mean sse))
    (lambda (&optional x)
      (if x
          (let ((previous-mean mean)
                (x (coerce x 'double-float)))
            (incf n)
            (incf mean (/ (- x mean) n))
            (incf sse (* (- x mean) (- x previous-mean))))
          (values sse mean n)))))

(defun weighted-sse-accumulator ()
  "Accumulator for weighted sum of squared errors.  When called without arguments,
return (values SSE SW MEAN N), where SW is the sum of weights."
  ;; West (1979)
  (declare (optimize speed))
  (let ((n 0)
        (mean 0d0)
        (sse 0d0)
        (sw 0d0))
    (declare (fixnum n)
             (double-float mean sse sw))
    (lambda (&optional x w)
      (if x
          (let ((previous-sw sw)
                (x (coerce x 'double-float))
                (w (coerce w 'double-float)))
            (incf n)
            (incf sw w)
            (let* ((q (- x mean))
                   (r (/ (* q w) sw)))
              (incf sse (* previous-sw q r))
              (incf mean r)))
          (values sse sw mean n)))))

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method ((sequence sequence))
    (apply-accumulator (mean-accumulator) sequence)))

(defgeneric sse (object &optional mean)
  (:documentation "Return the sum of (element-mean)^2 for each element in OBJECT.  If
  MEAN is not given, it is calculated and returned as a second value, with the number
  of elements as the third value.")
  (:method ((sequence sequence) &optional mean)
    (if mean
        (let ((mean (coerce mean 'double-float)))
          (reduce #'+ sequence 
                  :key (lambda (x) (expt (- (coerce x 'double-float) mean) 2))))
        (apply-accumulator (sse-accumulator) sequence)))
  (:method ((array array) &optional mean)
    (call-next-method (flatten-array array) mean)))

(defgeneric variance (object)
  (:documentation "Return the (sample or theoretial) variance.  If a second value is
  returned, that is the mean.")
  (:method (object)
    (bind (((:values ss nil n) (sse object)))
      (/ ss (1- n)))))

(defun mean-and-variance (object)
  "Return mean and variance as values."
  (multiple-value-bind (variance mean) (variance object)
    (values (if mean mean (mean object)) variance)))

(defgeneric weighted-mean (object weights)
  (:documentation "Return the weighted sample mean.")
  (:method ((sequence sequence) (weights sequence))
    (apply-accumulator (weighted-mean-accumulator) sequence weights)))

(defgeneric weighted-variance (object weights)
  (:documentation "Return the weighted sample mean.  If there are second and third
  values, they are the mean and the sum of weights.")
  (:method ((sequence sequence) (weights sequence))
    (bind (((:values sse sw mean)
            (apply-accumulator (weighted-sse-accumulator) sequence weights)))
      (values (/ sse (1- sw)) mean))))

(defun weighted-mean-and-variance (object weights)
  "Return weighted mean and variance as values."
  (multiple-value-bind (variance mean) (weighted-variance object weights)
    (values (if mean mean (weighted-mean object weights)) variance)))

;;; statistics

(defgeneric matrix-mean (matrix)
  (:documentation "Mean of a matrix, columnwise.")
  (:method ((matrix array))
    (bind ((dimensions (array-dimensions matrix))
           (means (filled-array (second dimensions) #'mean-accumulator)))
      (row-major-loop (dimensions row-major-index row-index col-index)
        (funcall (aref means col-index)
                 (row-major-aref matrix row-major-index)))
      (map 'vector #'funcall means))))

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

(defun sample-quantiles (vector quantiles &key destructive? sorted?
                         (element-type t))
  "Empirical quantiles of VECTOR.  QUANTILES has to be a sequence, and the
result is of the same type.  Elements are interpolated linearly.  If SORTED?,
copying and sorting is skipped (and of course the vector is not modified).  If
DESTRUCTIVE?, vector is sorted in place.  Element-type can be used to specify
resulting element-type."
  (check-type vector vector)
  (bind ((vector (cond
                   (sorted? vector)
                   (destructive? (sort vector #'<=))
                   (t (sort (copy-seq vector) #'<=))))
         (n (length vector))
         ((:flet quantile (q))
          (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
           (bind ((r (* q (1- n)))
                  ((:values int frac) (floor r))
                  (left (aref vector int)))
             (if (zerop frac)
                 left
                 (convex-combination left (aref vector (1+ int)) frac)))))
    (map `(simple-array ,element-type (*)) #'quantile quantiles)))
