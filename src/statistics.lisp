;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;; (define-condition not-enough-elements (error)
;;   ())

;;; primitives, also useful for for building defaults

;; (defgeneric size (object)
;;   (:documentation "Number of elements in object."))

;; (defgeneric sum (object)
;;   (:documentation "Sum of elements in object."))

;; (defgeneric sse (object &optional mean)
;;   (:documentation "Sum of squared errors.  When MEAN is given, it is
;;   used directly."))

;;; mean and variance, can be specialized, but have sensible default
;;; definitions

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method ((sequence sequence))
    ;; Welford's online algorithm
    (let ((n 0)
          (mean 0))
      (map nil (lambda (x)
                 (incf n)
                 (incf mean (/ (- x mean) n)))
           sequence)
      mean)))

(defgeneric sum-of-squared-errors (object &optional mean)
  (:documentation "Return the sum of (element-mean)^2 for each element in OBJECT.  If
  MEAN is not given, it is calculated and returned as a second value, with the number
  of elements as the third value.")
  (:method ((sequence sequence) &optional mean)
    (if mean
        (reduce #'+ sequence :key (lambda (x) (expt (- x mean) 2)))
        (let ((n 0)
              (mean 0)
              (ss 0))
          ;; Welford's online algorithm
          (map nil (lambda (x)
                     (incf n)
                     (let ((previous-mean mean))
                       (incf mean (/ (- x mean) n))
                       (incf ss (* (- x mean) (- x previous-mean)))))
               sequence)
          (values ss mean n)))))

(defgeneric sum-of-squares (object)
  (:documentation "Return the sum of squared elements.")
  (:method ((vector vector))
    (reduce #'+ vector :key (rcurry #'expt 2)))
  (:method ((array array))
    (sum-of-squares% (flatten-array array))))

(defgeneric variance (object)
  (:documentation "Return the (sample or theoretial) variance.  If a second value is
  returned, that is the mean.")
  (:method ((sequence sequence))
    (bind (((:values ss nil n) (sum-of-squared-errors sequence)))
      (/ ss (1- n)))))

(defun mean-and-variance (object)
  "Return mean and variance as values."
  (multiple-value-bind (variance mean) (variance object)
    (values (if mean mean (mean object)) variance)))

(defgeneric weighted-mean (object weights)
  (:documentation "Return the weighted sample mean.")
  (:method ((sequence sequence) weights)
    ;; West (1979)
    (let ((n 0)
          (mean 0)
          (sw 0))
      (map nil (lambda (x w)
                 (incf n)
                 (incf sw w)
                 (incf mean (/ (* (- x mean) w) sw)))
           sequence weights)
      mean)))

(defgeneric weighted-variance (object weights)
  (:documentation "Return the weighted sample mean.  If there are second and third
  values, they are the mean and the sum of weights.")
  (:method ((sequence sequence) weights)
    ;; West (1979)
    (let ((n 0)
          (mean 0)
          (ss 0)
          (sw 0))
      (map nil (lambda (x w)
                 (incf n)
                 (let ((previous-sw sw))
                   (incf sw w)
                   (let* ((q (- x mean))
                          (r (/ (* q w) sw)))
                     (incf ss (* previous-sw q r))
                     (incf mean r))))
           sequence weights)
      (values (/ ss (1- sw)) mean sw))))

(defun weighted-mean-and-variance (object weights)
  "Return weighted mean and variance as values."
  (multiple-value-bind (variance mean) (weighted-variance object weights)
    (values (if mean mean (weighted-mean object weights)) variance)))


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
