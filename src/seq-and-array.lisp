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
  "Return a sequence between FROM and TO, progressing by BY, of the given
LENGTH.  Only 3 of these a parameters should be given, the missing one (NIL)
should be inferred automatically.  The sign of BY is adjusted if necessary.  If
TYPE is LIST, the result is a list, otherwise it determines the element type of
the resulting simple array.  If TYPE is nil, it as autodetected from the
arguments (as a FIXNUM, a RATIONAL, or some subtype of FLOAT).  Note that your
implementation may upgrade the element type."
  (flet ((seq% (from by length)
           (if (eq type 'list)
               (iter
                 (for i :from 0 :below length)
                 (collecting (+ from (* i by))))
               (bind ((type (cond
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

(defun concat (&rest vectors)
  "Concatenate VECTORS in to a single vector.  Lists are treated as
SIMPLE-VECTORS.  The resulting array element type is found using
COMMON-SUPERTYPE."
  (let* ((vectors (mapcar (lambda (v)
                            (etypecase v
                              (vector v)
                              (list (coerce v 'simple-vector))))
                          vectors))
         (common-type (reduce #'common-supertype vectors :key #'array-element-type))
         (lengths (mapcar #'length vectors))
         (result (make-array (reduce #'+ lengths) :element-type common-type)))
    (iter
      (with offset := 0)
      (for v :in vectors)
      (for l :in lengths)
      (setf (subseq result offset (+ offset l)) v)
      (incf offset l))
    result))

(defun displace-array (array dimensions &optional (offset 0))
  "Shorthand function for displacing an array."
  (make-array dimensions :displaced-to array :displaced-index-offset offset
              :element-type (array-element-type array)))
