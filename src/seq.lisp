;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun vector* (element-type &rest elements)
  "Return a (SIMPLE-ARRAY ELEMENT-TYPE (*)) containing elements,
coerced to ELEMENT-TYPE."
  (let ((vector (make-array (length elements) :element-type element-type)))
    (iter
      (for i :from 0)
      (for element :in elements)
      (setf (aref vector i) (coerce element element-type)))
    vector))

(defun numseq (from to &key length (by 1 by?) type)
  "Return a sequence between FROM and TO, progressing by BY, of the
given LENGTH.  Only 3 of these a parameters should be given, the
missing one (NIL) should be inferred automatically.  If TYPE, it will
be the element type of the resulting SIMPLE-ARRAY, otherwise the
result is a LIST.  Note: the sign of BY is adjusted if necessary."
  (flet ((seq% (from by length)
           (if type
               (let ((result (make-array length :element-type type)))
                 (dotimes (i length)
                   (setf (aref result i) (coerce (+ from (* i by)) type)))
                 result)
               (iter
                 (for i :from 0 :below length)
                 (collecting (+ from (* i by)))))))
    (cond
      ((not from)
       (seq% (- to (* by (1- length))) by length))
      ((not to)
       (seq% from by length))
      ((not length)
       (assert (not (zerop by)))
       (let* ((range (- to from))
	      (by (* (signum range) (signum by) by))
              (length (1+ (floor (/ range by)))))
         (seq% from by length)))
      ((and length (not by?))
       (let ((range (- to from)))
         (seq% from (if (zerop range)
                        0
                        (/ range (1- length)))
               length)))
      (t (error "Only 3 of FROM, TO, LENGTH and BY are needed.")))))

(defun vector-satisfies? (vector predicate)
  "Return non-nil iff vector satisfies predicate elementwise.
Example: (vector-elementwise? vector #'<) tests if vector is strictly
increasing."
  (check-type vector vector)
  (when (< (length vector) 2)
    (return-from vector-satisfies? t))
  (iter
    (for element :in-vector vector :from 1)
    (for element-p :previous element :initially (aref vector 0))
    (always (funcall predicate element-p element))))

(defgeneric cumsum (object)
  (:documentation "The cumulative sum of the elements.  Always starts
  with the first element, and ends with the total."))

(defmethod cumsum ((sequence sequence))
  (let ((sum 0))
    (map (type-of sequence) (lambda (element)
                              (incf sum element))
         sequence)))
