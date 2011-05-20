;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(deftype simple-fixnum-vector ()
  '(simple-array fixnum (*)))

(defun as-simple-fixnum-vector (sequence &optional copy?)
  "Convert SEQUENCE to a SIMPLE-FIXNUM-VECTOR.  When COPY?, make sure that the
they don't share structure."
  (if (and (typep sequence 'simple-fixnum-vector) copy?)
      (copy-seq sequence)
      (coerce sequence 'simple-fixnum-vector)))

(defun array* (dimensions element-type &rest elements)
  "Return a (SIMPLE-ARRAY ELEMENT-TYPE dimensions) containing ELEMENTS,
coerced to ELEMENT-TYPE."
  (aprog1 (make-array dimensions :element-type element-type)
    (dotimes (index (array-total-size it))
      (assert elements () "Not enough elements.")
      (setf (row-major-aref it index) (coerce (car elements) element-type)
            elements (cdr elements)))
    (assert (not elements) () "Too many elements (~A)." elements)))

(defun vector* (element-type &rest elements)
  "Return a (SIMPLE-ARRAY ELEMENT-TYPE (*)) containing ELEMENTS,
coerced to ELEMENT-TYPE."
  (apply #'array* (length elements) element-type elements))

(defun filled-array (dimensions function &optional (element-type t))
  "Create array with given DIMENSIONS and ELEMENT-TYPE, then fill by calling
FUNCTION, traversing in row-major order."
  (aprog1 (make-array dimensions :element-type element-type)
    (dotimes (index (array-total-size it))
      (setf (row-major-aref it index) (funcall function)))))

;;; !! define compiler macros for VECTOR* and ARRAY*

(defun iseq (n &optional (type 'fixnum))
  "Return a sequence of integers.  If type is LIST, a list is returned,
otherwise a vector with the corresponding upgraded element type."
  (if (eq type 'list)
      (loop for i below n collect i)
      (aprog1 (make-array n :element-type type)
        (dotimes (i n)
          (setf (aref it i) (coerce i type))))))

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

(defun map-array (function array &optional (element-type t))
  "Map array elementwise."
  (aprog1 (make-array (array-dimensions array) :element-type element-type)
    (dotimes (index (array-total-size array))
      (setf (row-major-aref it index)
            (funcall function (row-major-aref array index))))))

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

(defun cumulative-sum (sequence &key 
                       (result-type 
                        (etypecase sequence
                          (list 'list)
                          (vector `(simple-array ,(array-element-type sequence) (*))))))
  "Cumulative sum of sequence.  Return a sequence of the same kind and length; last
element is the total.  The total is returned as the second value."
  (let ((sum 0))
    (values (map result-type (lambda (element)
                               (incf sum element))
                 sequence)
            sum)))

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

(defun make-similar-array (array &key (dimensions (array-dimensions array)) (initial-element nil initial-element?))
  "Make a simple-array with the given dimensions and element-type
similar to array."
  (let ((element-type (array-element-type array)))
    (if initial-element?
        (make-array dimensions :element-type element-type
                    :initial-element (coerce initial-element element-type))
        (make-array dimensions :element-type element-type))))

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
         (result (make-similar-array vector :dimensions (* n times each)))
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

(defun flatten-array (array &key copy?)
  "Return ARRAY flattened to a vector, copyied if COPY?, otherwise displaced."
  (let ((vector (displace-array array (array-total-size array))))
    (if copy? (copy-seq vector) vector)))

(defun displace-subarray (array &rest subscripts)
  "Given a partial list of subscripts, return a displaced array that starts
  there, with all the other subscripts set to 0, dimensions inferred from the
  original.  If no subscripts are given, the original array is returned."
  (let* ((rank (array-rank array))
         (drop (length subscripts)))
    (cond
      ((zerop drop) array)
      ((<= drop rank)
       (let ((sub-dimensions (subseq (array-dimensions array) drop))
             (offset (apply #'array-row-major-index array
                            (append subscripts
                                    (make-sequence 'list (- rank drop)
                                                   :initial-element 0)))))
         (displace-array array sub-dimensions offset)))
      (t (error "Too many subscripts (~A) for array of rank ~A." drop rank)))))

(defun group (sequence &rest indexes)
  "Return an array, which contains sequences of the same type as SEQUENCE, with
elements grouped according to the indexes (which are expected to be nonnegative
fixnums).  The maximum index for each dimension is calculated automatically, and
they detemine the dimensions of the result.  Order of the elements is preserved."
  (unless indexes
    (return-from group sequence))
  (let* ((indexes (mapcar (lambda (index) (coerce index 'vector)) indexes))
         (dimensions (mapcar (lambda (index) (1+ (reduce #'max index))) indexes))
         (result (make-array dimensions :initial-element nil))
         (length (length sequence)))
    (assert (every (lambda (index) (= (length index) length)) indexes) ()
            "Indexes must be the same length as the sequence.")
    (map nil
         (let ((i 0))
           (lambda (element)
             (push element
                   (apply #'aref result 
                          (mapcar (lambda (index) (aref index i)) indexes)))
             (incf i)))
         sequence)
    (let ((sequence-type
           (etypecase sequence
             (list 'list)
             (vector `(simple-array ,(array-element-type sequence) (*))))))
      (dotimes (i (array-total-size result))
        (setf (row-major-aref result i)
              (nreverse (coerce (row-major-aref result i) sequence-type)))))
    result))

(declaim (inline boolean-to-bit predicate-as-flag))

(defun boolean-to-bit (boolean)
  "Convert a boolean to a bit."
  (if boolean 1 0))

(defun predicate-as-flag (predicate)
  "For convert a predicate to a function that returns a bit."
  (lambda (object) (boolean-to-bit (funcall predicate object))))

(defun positions (bit-vector)
  "Return the indexes for nonzero elements in increasing order."
  (check-type bit-vector bit-vector)
  (iter
    (for element :in-vector bit-vector :with-index position)
    (unless (zerop element)
      (collect position :result-type simple-fixnum-vector))))

(defun which (predicate sequence)
  "Map sequence into a simple-bit-vector, using 1 when PREDICATE yields true, 0
otherwise."
  (map 'simple-bit-vector (predicate-as-flag predicate) sequence))

(defun which-positions (predicate sequence)
  "Return an index of the positions in SEQUENCE which satisfy PREDICATE."
  (let ((index 0)
        positions)
    (map nil (lambda (element)
               (when (funcall predicate element)
                 (push index positions))
               (incf index))
         sequence)
    (coerce (nreverse positions) 'simple-fixnum-vector)))

(defgeneric which-rows (predicate object)
  (:documentation "Return a simple-bit-vector, flagging the rows of a matrix 
using predicate."))

(defmethod which-rows (predicate (matrix array))
  (bind (((n-row nil) (array-dimensions matrix))
         (flag (predicate-as-flag predicate))
         (result (make-array n-row :element-type 'bit)))
    (dotimes (row-index n-row)
      (setf (aref result row-index)
            (funcall flag (displace-subarray matrix row-index))))
    result))

;;; !!! compiler macros for (support (which...)), or maybe even name them

(defmacro define-vector-accessors (&optional (n 10))
  (flet ((accessor-name (i)
           (intern (format nil "~:@(~:r~)*" i))))
    `(progn
       ,@(loop for i from 1 to n
               collect 
               `(defun ,(accessor-name i) (array)
                  (row-major-aref array ,(1- i))))
       (declaim (inline ,@(loop for i from 1 to n
                                collect (accessor-name i)))))))

(define-vector-accessors)

(defmacro row-major-loop ((dimensions row-major-index row-index col-index
                                      &key (nrow (gensym* '#:nrow))
                                           (ncol (gensym* '#:ncol)))
                          &body body)
  "Loop through row-major matrix with given DIMENSIONS, incrementing
ROW-MAJOR-INDEX, ROW-INDEX and COL-INDEX."
  (check-types (row-index col-index row-major-index nrow ncol) symbol)
  `(bind (((,nrow ,ncol) ,dimensions)
          (,row-major-index 0))
     (dotimes (,row-index ,nrow)
       (dotimes (,col-index ,ncol)
         ,@body
         (incf ,row-major-index)))))
