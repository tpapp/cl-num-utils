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

(defun sort-order (sequence predicate &key key)
  "Sort elements of SEQUENCE using PREDICATE (and optionally, KEY).  Return the
permutation of elements as the second value (which is always a (SIMPLE-ARRAY
FIXNUM (*)).  Functional and nondestructive."
  (let* ((index 0)
         (paired (map 'simple-vector
                      (lambda (element)
                        (prog1 (cons element index)
                          (incf index)))
                      sequence))
         (paired (sort paired predicate
                       :key (if key (compose key #'car) #'car))))
    (values (map (type-of sequence) #'car paired)
            (map '(simple-array fixnum (*)) #'cdr paired))))

(defun make-similar-vector (vector &optional (length (array-total-size vector)))
  "Make a simple-array1 with the given lengh and element-type similar
to vector."
  (make-array length :element-type (array-element-type vector)))

(defun make-similar-array (array &optional (dimensions (array-dimensions array)))
  "Make a simple-array with the given dimensions and element-type
similar to array."
  (make-array dimensions :element-type (array-element-type array)))

(defgeneric rep (sequence times &optional each)
  (:documentation "Return a new sequence, which contains SEQUENCE repeated TIMES
times, repeating each element EACH times (default is 1)."))

(defmethod rep ((list list) times &optional (each 1))
  (iter :outer
    (repeat times)
    (iter
      (for elt :in list)
      (iter
        (repeat each)
        (in :outer 
            (collecting elt))))))

(defmethod rep ((vector vector) times &optional (each 1))
  (let* ((n (length vector))
         (result (make-similar-vector vector (* n times each)))
         (result-index 0))
    (dotimes (outer times)
      (dotimes (vector-index n)
        (let ((elt (aref vector vector-index)))
          (dotimes (inner each)
            (setf (aref result result-index) elt)
            (incf result-index)))))
    result))



(rep '(1 2 3) 4 2)
(rep #(1 2 3) 4 2)
