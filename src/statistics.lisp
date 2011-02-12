;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(define-condition not-enough-elements (error)
  ())

;;; primitives, also useful for for building defaults

(defgeneric size (object)
  (:documentation "Number of elements in object."))

(defgeneric sum (object)
  (:documentation "Sum of elements in object."))

(defgeneric sse (object &optional mean)
  (:documentation "Sum of squared errors.  When MEAN is given, it is
  used directly."))

;;; mean and variance, can be specialized, but have sensible default
;;; definitions

(defgeneric mean (object)
  (:documentation "Return the mean.")
  (:method (object)
     (/ (sum object) (size object))))

(defun sample-second-moment% (sse length)
  "Calculate variance from SSE and LENGTH.  Meant for defining new
variance methods."
  (if (< 1 length)
      (/ sse (1- length))
      (error 'not-enough-elements)))

(defun sample-var (object &optional (mean (mean object)))
  "Return the sample variance.  When the object doesn't have enough
  elements, an error is signalled.  The mean will be used if supplied."
  (sample-second-moment% (sse object mean) (size object)))

(defun sample-sd (object &optional (mean (mean object)))
  "Return the sample standard deviation."
  (sqrt (sample-var object mean)))

(defun sample-cov (a b &key (a-mean (mean a)) (b-mean (mean b)))
  "Sample covariance between A and B.  Means will be used when provided."
  (let* ((a (coerce a 'vector))
         (b (coerce b 'vector))
         (size (length a))
         (sum 0))
    (assert (= (length b) size) () "Vectors do not have the same length.")
    (dotimes (i size)
      (incf sum (* (- (aref a i) a-mean)
                   (- (aref b i) b-mean))))
    (sample-second-moment% sum size)))

(defun sample-corr (a b &key (a-mean (mean a)) (b-mean (mean b)))
  "Sample correlation between A and B.  Means will be used when provided."
  (/ (sample-cov a b :a-mean a-mean :b-mean b-mean)
     (sample-sd a a-mean) (sample-sd b b-mean)))

;;; sensible behavior for sequences and arrays

(defmethod size ((sequence sequence))
  (length sequence))

(defmethod size ((array array))
  (array-total-size array))

(defmethod sum ((sequence sequence))
  (reduce #'+ sequence))

(defmethod sum ((array array))
  (iter
    (for index :from 0 :below (array-total-size array))
    (summing (row-major-aref array index))))

(defmethod sse ((sequence sequence) &optional (mean (mean sequence)))
  (reduce #'+ sequence :key (lambda (x) (expt (- x mean) 2))))

(defmethod sse ((array array) &optional (mean (mean array)))
  (iter
    (for index :from 0 :below (array-total-size array))
    (summing (expt (- (row-major-aref array index) mean) 2))))

(defun sample-quantiles (vector quantiles &key destructive? sorted?)
  "Empirical quantiles of VECTOR.  QUANTILES has to be a sequence, and the
result is of the same type.  Elements are interpolated linearly.  If SORTED?,
copying and sorting is skipped (and of course the vector is not modified).  If
DESTRUCTIVE?, vector is sorted in place."
  (check-type vector vector)
  (let* ((vector (cond
                   (sorted? vector)
                   (destructive? (sort vector #'<=))
                   (t (sort (copy-seq vector) #'<=))))
         (n (length vector)))
    (map (type-of quantiles)
         (lambda (q)
           (assert (<= 0 q 1) () "Quantile ~A is not in [0,1]." q)
           (bind ((r (* q (1- n)))
                  ((:values int frac) (floor r))
                  (left (aref vector int)))
             (if (zerop frac)
                 left
                 (convex-combination left (aref vector (1+ int)) frac))))
         quantiles)))
