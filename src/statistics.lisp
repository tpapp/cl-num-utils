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

(defun variance% (sse length)
  "Calculate variance from SSE and LENGTH.  Meant for defining new
variance methods."
  (if (< 1 length)
      (/ sse (1- length))
      (error 'not-enough-elements)))

(defgeneric variance (object)
  (:documentation "Return the variance.  When the object doesn't have
  enough elements, DEFAULT is returned when given, otherwise an error
  is signalled.")
  (:method (object)
     (bind (((:accessors-r/o sse size) object))
       (variance% sse size))))

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
