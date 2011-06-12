;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; rewrite this as
;;; 
;;; (iseq below) (iseq from below) (iseq from by below)
;;; 
;;; (aseq :from :to :length)
;;; (aseq :from :by :length)

(defun numseq (from to &key length (by 1 by?) type)
  "Return a sequence between FROM and TO, progressing by BY, of the given
LENGTH.  Only 3 of these a parameters should be given, the missing one (NIL)
should be inferred automatically.  The sign of BY is adjusted if necessary.
If TYPE is LIST, the result is a list, otherwise it determines the element
type of the resulting simple array.  If TYPE is nil, it as autodetected from
the arguments (as a FIXNUM, a RATIONAL, or some subtype of FLOAT).  Note that
your implementation may upgrade the element type."
  (flet ((seq% (from by length)
           (if (eq type 'list)
               (iter
                 (for i :from 0 :below length)
                 (collecting (+ from (* i by))))
               (let+ ((type (cond
                              (type type)
                              ((= length 1) (if (typep from 'fixnum)
                                                'fixnum
                                                (type-of from)))
                              (t (let ((to (+ from (* by length))))
                                   (etypecase to
                                     (fixnum (if (typep from 'fixnum)
                                                 'fixnum
                                                 'integer))
                                     (float (type-of to))
                                     (t 'rational))))))
                      (result (make-array length :element-type type)))
                 (dotimes (i length)
                   (setf (aref result i) (coerce (+ from (* i by)) type)))
                 result))))
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

(defun ivec (end-or-start &optional (end 0 end?) (by 1) strict-direction?)
  "Return a vector of fixnums.

   (ivec end) =>  #(0 ... end-1) (or #(0 ... end+1) when end is negative).
   (ivec start end) => #(start ... end-1) or to end+1 when end is negative.

   When BY is given it determines the increment, adjusted to match the
   direction unless STRICT-DIRECTION, in which case an error is signalled. "
  (check-types (end-or-start end by) fixnum)
  (if end?
      (let* ((abs-by (abs by))
             (start end-or-start)
             (diff (- end start))
             (length (ceiling (abs diff) abs-by))
             (by (aprog1 (* abs-by (signum diff))
                   (when strict-direction?
                     (assert (= it by) () "BY does not match direction."))))
             (element start))
        (aprog1 (make-array length :element-type 'fixnum)
          (loop for index below length
                do (setf (aref it index) element)
                   (incf element by))))
      (let* ((end end-or-start)
             (abs-end (abs end)))
        (aprog1 (make-array abs-end :element-type 'fixnum)
          (if (plusp end)
              (loop for index below abs-end
                    do (setf (aref it index) index))
              (loop for index below abs-end
                    do (setf (aref it index) (- index))))))))

(defun similar-element-type (element-type)
  "Return a type that is a supertype of ELEMENT-TYPE and is closed under
arithmetic operations.  May not be the narrowest."
  (if (subtypep element-type 'float)
      element-type
      t))

(defun similar-sequence-type (sequence)
  "Return type that sequence can be mapped to using arithmetic operations."
  (etypecase sequence
    (list 'list)
    (vector `(simple-array 
              ,(similar-element-type (array-element-type sequence)) (*)))))

(defgeneric sum (object &key key)
  (:documentation "Sum of elements in object.  KEY is applied to each
  element.")
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

(defun cumulative-sum (sequence
                       &key (result-type (similar-sequence-type sequence)))
  "Cumulative sum of sequence.  Return a sequence of the same kind and length;
last element is the total.  The latter is returned as the second value."
  (let ((sum 0))
    (values (map result-type (lambda (element)
                               (incf sum element))
                 sequence)
            sum)))

(defun cumulative-product (sequence 
                           &key (result-type
                                 (similar-sequence-type sequence)))
  "Cumulative product of sequence.  Return a sequence of the same kind and
length; last element is the total product.  The latter is also returned as the
second value."
  (let ((sum 0))
    (values (map result-type (lambda (element)
                               (incf sum element))
                 sequence)
            sum)))

