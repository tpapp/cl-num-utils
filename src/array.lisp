;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; utility functions

(defun displace-array (array dimensions &optional (offset 0))
  "Shorthand function for displacing an array."
  (make-array dimensions
              :displaced-to array
              :displaced-index-offset offset
              :element-type (array-element-type array)))

(defun flatten-array (array &key copy?)
  "Return ARRAY flattened to a vector.  Will share structure unless COPY?."
  (let ((vector (displace-array array (array-total-size array))))
    (if copy? (copy-seq vector) vector)))

(defun common-array-element-type (objects &key (key #'array-element-type))
  "Return the common upgraded element type of objects (eg arrays with the
default key).  Not guaranteed to be the narrowest type (depends on the
implementation), stops when it reaches T."
  (reduce (lambda (t1 t2)
            (when (eq t1 t)
              (return-from common-array-element-type t))
            (cond
              ((subtypep t1 t2) t2)
              ((subtypep t2 t1) t1)
              (t (upgraded-array-element-type `(or ,t1 ,t2)))))
          objects :key key))

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
                           &key (nrow (gensym "nrow"))
                                (ncol (gensym "ncol")))
                          &body body)
  "Loop through row-major matrix with given DIMENSIONS, incrementing
ROW-MAJOR-INDEX, ROW-INDEX and COL-INDEX."
  (check-types (row-index col-index row-major-index nrow ncol) symbol)
  `(let+ (((,nrow ,ncol) ,dimensions)
          (,row-major-index 0))
     (dotimes (,row-index ,nrow)
       (dotimes (,col-index ,ncol)
         ,@body
         (incf ,row-major-index)))))

(defun array-element-type-available (type)
  "Return a boolean indicating whether TYPE upgraded to itself for arrays.
  Naturally, the result is implementation-dependent and constant within the
  same implementation."
  (type= (upgraded-array-element-type type) type))

(defgeneric nrow (object)
  (:documentation "Return number of rows in object.  Signal an error if OBJECT
  doesn't have exactly two dimensions.")
  (:method ((array array))
    (assert (= 2 (array-rank array)) () "Array is not a matrix.")
    (array-dimension array 0)))

(defgeneric ncol (object)
  (:documentation "Return number of columns in object.  Signal an error if
  OBJECT doesn't have exactly two dimensions.")
  (:method ((array array))
    (assert (= 2 (array-rank array)) () "Array is not a matrix.")
    (array-dimension array 1)))

(defgeneric elements (object)
  (:documentation "Return elements from object.  May share structure."))

(defun square? (matrix)
  "Test if a matrix (in the generalized sense, ie an object that has nrow and
ncol) is square."
  (= (nrow matrix) (ncol matrix)))

(deftype matrix (&optional (element-type '*) (nrow '*) (ncol '*))
  "Array of rank 2."
  `(array ,element-type (,nrow ,ncol)))

(declaim (inline seqs-to-matrix* seqs-to-matrix matrix* matrix))

(defun seqs-to-matrix* (element-type sequences)
  "Create a matrix from sequences with given element type."
  (make-array (list (length sequences) (length (first sequences)))
              :element-type element-type :initial-contents sequences))

(defun seqs-to-matrix (sequences)
  "Create a matrix with element-type T."
  (seqs-to-matrix* t sequences))

(defun matrix* (element-type &rest sequences)
  "Create a matrix from sequences with given element type."
  (seqs-to-matrix* element-type sequences))

(defun matrix (&rest sequences)
  "Create a matrix with element-type T."
  (seqs-to-matrix sequences))

(defun vector* (element-type &rest objects)
  (make-array (length objects) :element-type element-type
                               :initial-contents objects))

(defun make-similar-array (array
                           &key (dimensions (array-dimensions array))
                                (initial-element nil initial-element?))
  "Make a simple-array with the given dimensions and element-type
similar to array."
  (let ((element-type (array-element-type array)))
    (if initial-element?
        (make-array dimensions :element-type element-type
                               :initial-element (coerce initial-element element-type))
        (make-array dimensions :element-type element-type))))

(declaim (inline uniform-array))
(defun uniform-array (dimensions value &optional (element-type t))
  "Array filled with VALUE."
  (make-array dimensions :element-type element-type :initial-element value))

(defun generate-array (dimensions function-or-value &optional (element-type t))
  "Create array with given DIMENSIONS and ELEMENT-TYPE, then fill by calling
FUNCTION (traversing in row-major order) or using VALUE."
  (if (functionp function-or-value)
      (aprog1 (make-array dimensions :element-type element-type)
        (dotimes (index (array-total-size it))
          (setf (row-major-aref it index) (funcall function-or-value))))
      (uniform-array dimensions function-or-value element-type)))

(defmethod rep (vector times &optional (each 1))
  "Return a new sequence, which contains SEQUENCE repeated TIMES times,
repeating each element EACH times (default is 1)."
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

;;; reshape

(defun fill-in-dimensions (dimensions size)
  "If one of the dimensions is missing (indicated with T), replace it with a
dimension so that the total product equals SIZE.  If that's not possible,
signal an error.  If there are no missing dimensions, just check that the
product equals size."
  (let+ ((dimensions (ensure-list dimensions))
         ((&flet missing? (dimension) (eq dimension t)))
         missing
         (product 1))
    (mapc (lambda (dimension)
            (if (missing? dimension)
                (progn
                  (assert (not missing) () "More than one missing dimension.")
                  (setf missing t))
                (progn
                  (check-type dimension (integer 1))
                  (multf product dimension))))
          dimensions)
    (if missing
        (let+ (((&values fraction remainder) (floor size product)))
          (assert (zerop remainder) ()
                  "Substitution does not result in an integer.")
          (mapcar (lambda (dimension)
                    (if (missing? dimension) fraction dimension))
                  dimensions))
        dimensions)))

(defun reshape (dimensions array &key (offset 0) copy?)
  "Reshape ARRAY using DIMENSIONS, one of which may be T which is calculated
on demand."
  (let* ((size (array-total-size array))
         (dimensions (fill-in-dimensions dimensions (- size offset))))
    (maybe-copy-array (displace-array array dimensions offset) copy?)))

;;; subarrays

(defun subarrays (rank array)
  "Return an array of subarrays, split of at RANK.  All subarrays are
displaced and share structure."
  (let ((array-rank (array-rank array)))
    (cond
      ((or (zerop rank) (= rank array-rank))
       array)
      ((< 0 rank array-rank)
       (let* ((dimensions (array-dimensions array))
              (result
                (make-array (subseq dimensions 0 rank)))
              (sub-dimensions (subseq dimensions rank))
              (sub-size (product sub-dimensions)))
         (dotimes (index (array-total-size result))
           (setf (row-major-aref result index)
                 (displace-array array sub-dimensions
                                 (* index sub-size))))
         result))
      (t (error "Rank ~A outside [0,~A]." rank array-rank)))))

(defun subarray-location (dimensions subscripts)
  "Return (values OFFSET REMAINING-DIMENSIONS) that can be used to displace a
row-major subarray starting at SUBSCRIPTS in an array with the given
DIMENSIONS."
  (let+ (rev-dimensions
         rev-subscripts
         (tail (do ((dimensions dimensions (cdr dimensions))
                    (subscripts subscripts (cdr subscripts)))
                   ((not subscripts) dimensions)
                 (assert dimensions ()
                         "More subscripts than dimensions.")
                 (let ((s (car subscripts))
                       (d (car dimensions)))
                   (declare (type fixnum d))
                   (assert (and (fixnum? s) (< -1 s d)) ()
                           "Invalid subscript.")
                   (push s rev-subscripts)
                   (push d rev-dimensions))))
         (product (reduce #'* tail))
         (sum 0))
    (declare (type fixnum product sum))
    (mapc (lambda (d s)
            (declare (type fixnum d s))
            (incf sum (the fixnum (* product s)))
            (multf product d))
          rev-dimensions rev-subscripts)
    (values sum tail)))

(defun subarray (array &rest subscripts)
  "Given a partial list of subscripts, return the subarray that starts there,
with all the other subscripts set to 0, dimensions inferred from the original.
If no subscripts are given, the original array is returned.  Implemented by
displacing, shares structure unless the second value is true, which indicates
that a single element was returned (ie subarray was equivalent to aref)."
  (if subscripts
      (let+ (((&values offset dimensions)
              (subarray-location (array-dimensions array) subscripts)))
        (if dimensions
            (displace-array array dimensions offset)
            (values (apply #'aref array subscripts) t)))
      array))

(defun (setf subarray) (value array &rest subscripts)
  (let+ (((&values subarray atom?) (apply #'subarray array subscripts)))
    (if atom?
        (setf (apply #'aref array subscripts) value)
        (prog1 value
          (assert (common-dimensions value subarray))
          (replace (flatten-array subarray) (flatten-array value))))))

(defun partition (array start &optional end)
  "Return a subset of the array, on the first indexes between START and END."
  (let* ((d0 (array-dimension array 0))
         (stride (/ (array-total-size array) d0)))
    (unlessf end d0)
    (assert (and (<= 0 start) (< start end) (<= end d0)))
    (displace-array array (cons (- end start) (cdr (array-dimensions array)))
                    (* start stride))))

(defun combine (array &optional element-type)
  "The opposite of SUBARRAYS.  If ELEMENT-TYPE is not given, it is inferred
from the first element of array, which also determines the dimensions.  If
that element is not an array, the original ARRAY is returned as it is."
  (let ((first (first* array)))
    (if (arrayp first)
        (let* ((dimensions (array-dimensions array))
               (sub-dimensions (array-dimensions first))
               (element-type (aif element-type it (array-element-type first)))
               (result (make-array (append dimensions sub-dimensions)
                                   :element-type element-type))
               (length (product dimensions))
               (displaced (displace-array result
                                          (cons length sub-dimensions))))
          (dotimes (index length)
            (setf (subarray displaced index) (row-major-aref array index)))
          result)
        array)))

(defun matrix-to-columns (matrix)
  "Columns of a matrix as a vector of vectors."
  (let+ (((nrow ncol) (array-dimensions matrix))
         (element-type (array-element-type matrix))
         (columns (make-array ncol)))
    (dotimes (col-index ncol)
      (setf (aref columns col-index)
            (aprog1 (make-array nrow :element-type element-type)
              (dotimes (row-index nrow)
                (setf (aref it row-index)
                      (aref matrix row-index col-index))))))
    columns))

(defun columns-to-matrix (columns
                          &optional (element-type
                                     (common-array-element-type columns)))
  "Convert columns (vectors of equal length) to matrix.  The resulting array's
ELEMENT-TYPE is deduced automatically, unless given."
  (let* ((ncol (length columns))
         (nrow (length (first* columns)))
         (result (make-array (list nrow ncol) :element-type element-type)))
    (dotimes (col-index ncol)
      (let ((column (aref columns col-index)))
        (unless (zerop col-index)
          (assert (length= column nrow) ()
                  "Column ~A has a different length." col-index))
        (dotimes (row-index nrow)
          (setf (aref result row-index col-index)
                (aref column row-index)))))
    result))

(defgeneric map1 (function object &key element-type &allow-other-keys)
  (:documentation "Map OBJECT elementwise using FUNCTION.  Results in a
  similar object, with specificed ELEMENT-TYPE where applicable.")
  (:method (function (array array) &key (element-type t))
    (aprog1 (make-array (array-dimensions array) :element-type element-type)
      (map-into (flatten-array it) function (flatten-array array))))
  (:method (function (list list) &key)
    (mapcar function list)))

;;; subvector

(defun subvector (vector start &optional (end (length vector)))
  "Displaced vector between START and END."
  (displace-array vector (- end start) start))

(declaim (inline (setf subvector)))
(defun (setf subvector) (value vector start &optional (end (length vector)))
  ;; just a synonym for (setf subseq), defined for symmetry
  (setf (subseq vector start end) value))

;; (defun map-subarrays (function array rank &optional element-type)
;;   "Map subarrays.  When ELEMENT-TYPE is given, it is used for the element type
;; of the result."
;;   (combine (map1 function (subarrays array rank)) element-type))

;;; generic interface for array-like objects

(defgeneric as-array (object &key copy? &allow-other-keys)
  (:documentation "(as-array object) always returns OBJECT as a Common Lisp
  which may nevertheless share structure with something.  COPY? can be used to
  avoid that.  Other keyword arguments may make as-array return something
  else (eg an array wrapped in a structure to indicate that it is special).")
  (:method ((array array) &key copy?)
    (maybe-copy-array array copy?)))

(defgeneric diagonal (object &key copy?)
  (:documentation "Return diagonal of object.")
  (:method ((matrix array) &key copy?)
    (declare (ignore copy?))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (n (min nrow ncol))
           (diagonal (make-similar-array matrix :dimensions n)))
      (dotimes (i n)
        (setf (row-major-aref diagonal i)
              (aref matrix i i)))
      diagonal)))

(defgeneric transpose (object &key copy?)
  (:documentation "Transpose a matrix.")
  (:method ((matrix array) &key copy?)
    (declare (ignore copy?))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (result (make-array (list ncol nrow)
                               :element-type (array-element-type matrix)))
           (result-index 0))
      (dotimes (col ncol)
        (dotimes (row nrow)
          (setf (row-major-aref result result-index) (aref matrix row col))
          (incf result-index)))
      result)))

(defgeneric transpose* (object &key copy?)
  (:documentation "Conjugate transpose a matrix.")
  (:method ((matrix array) &key copy?)
    (declare (ignore copy?))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (result (make-array (list ncol nrow)
                               :element-type (array-element-type matrix)))
           (result-index 0))
      (dotimes (col ncol)
        (dotimes (row nrow)
          (setf (row-major-aref result result-index)
                (conjugate (aref matrix row col)))
          (incf result-index)))
      result)))

(defun valid-permutation? (permutation &optional (rank (length permutation) rank?))
  "Test if PERMUTATION is a valid permutation (of rank RANK)."
  (let+ ((flags (make-array rank :element-type 'bit :initial-element 0))
         ((&flet invalid () (return-from valid-permutation? nil))))
    (when (and rank? (/= rank (length permutation))) (invalid))
    (map nil (lambda (p)
               (unless (within? 0 p rank) (invalid))
               (setf (aref flags p) 1)) permutation)
    (= (count 1 flags) rank)))

(defun permute (array permutation)
  "Permute array axes.  Elements ofthe sequence PERMUTATION indicate where
that particular axis is coming from in ARRAY."
  (let+ ((dimensions (as-simple-fixnum-vector (array-dimensions array)))
         (permutation (as-simple-fixnum-vector permutation))
         (rank (length dimensions))
         (counters (make-array rank :element-type 'fixnum :initial-element 0))
         (subscripts (make-list rank))
         (result (make-array (map 'list (lambda (p) (aref dimensions p))
                                  permutation))))
    (assert (valid-permutation? permutation rank))
    (dotimes (row-major-index (array-total-size array))
      ;; copy element
      (map-into subscripts (lambda (p) (aref counters p)) permutation)
      (setf (apply #'aref result subscripts)
            (row-major-aref array row-major-index))
      ;; increase counters
      (iter
        (for axis :from (1- rank) :downto 0)
        (if (= (incf (aref counters axis)) (aref dimensions axis))
            (setf (aref counters axis) 0)
            (finish))))
    result))

(defun as-row (vector &key copy?)
  "Return vector as a matrix with one row."
  (check-type vector vector)
  (maybe-copy-array (displace-array vector (list 1 (length vector))) copy?))

(defun as-column (vector &key copy?)
  "Return vector as a matrix with one column."
  (check-type vector vector)
  (maybe-copy-array (displace-array vector (list (length vector) 1)) copy?))

;;; outer product

(defun outer* (a b function &optional (element-type t))
  "Generalized outer product of A and B, using FUNCTION.  ELEMENT-TYPE can be
used to give the element type.  Also see LLA:OUTER."
  (check-types (a b) vector)
  (let* ((a-length (length a))
         (b-length (length b))
         (result (make-array (list a-length b-length)
                             :element-type element-type))
         (result-index 0))
    (map nil (lambda (a-element)
               (map nil (lambda (b-element)
                          (setf (row-major-aref result result-index)
                                (funcall function a-element b-element))
                          (incf result-index))
                    b))
         a)
    result))

;;; norms

;;; !! matrix norms would be nice, in that case we need to make these generic
;;; !! functions.

(defun norm1 (a)
  (reduce #'+ a :key #'abs))

(defun norm2 (a)
  "L2 norm."
  (sqrt (reduce #'+ a :key (lambda (x) (* (conjugate x) x)))))

(defun normsup (a)
  (reduce #'max a :key #'abs))

;;; iterate clause for columns

(defmacro-driver (for var in-columns matrix :with-index index)
  (with-unique-names (matrix-var ncol-var)
    (let ((kwd (if generate 'generate 'for)))
      `(progn
         (with ,matrix-var := ,matrix)
         (with ,ncol-var := (ncol ,matrix-var))
         (with ,index := -1)
         (,kwd ,var next (progn
                           (incf ,index)
                           (when (>= ,index ,ncol-var) (terminate))
                           (sub ,matrix-var t ,index)))))))

(defgeneric map-columns (function object &key element-type &allow-other-keys)
  (:documentation "Map columns of object (eg a matrix) using function.
FUNCTION is called with columns that are extracted as a vector, and the
returned vectors are assembled into another matrix.  Element types and number
of rows are established after the first function call, and are checked for
conformity after that -- when element-type is given, it is used instead.  If
the function doesn't return a vector, the values are collected in a vector
instead of a matrix.")
  (:method (function (matrix array) &key element-type)
    (let+ (((nil ncol) (array-dimensions matrix))
           result
           result-nrow)
      (loop
        for col-index :from 0 :below ncol
        do (let ((mapped-col (funcall function (sub matrix t col-index))))
             (when (zerop col-index)
               (if (vectorp mapped-col)
                   (setf result-nrow (length mapped-col)
                         result (make-array (list result-nrow ncol)
                                            :element-type
                                            (aif element-type
                                                 it
                                                 (array-element-type mapped-col))))
                   (setf result (make-array ncol))))
             (if result-nrow
                 (setf (sub result t col-index) mapped-col)
                 (setf (aref result col-index) mapped-col))))
      result)))

(defgeneric map-rows (function object &key element-type &allow-other-keys)
  (:documentation "Map rows of object (eg a matrix) using FUNCTION.")
  (:method (function (matrix array) &key (element-type t))
    (check-type matrix matrix)
    ;; FIXME: more efficient implementations should be possible
    (combine (map 'vector function (subarrays 1 matrix)) element-type)))

(defun recycle-row (vector nrow)
  "Return a matrix with NROW rows, each identical to vector."
  (make-array (list nrow (length vector))
              :element-type (array-element-type vector)
              :initial-contents (make-array nrow :initial-element vector)))

(defun recycle-column (vector ncol)
  "Return a matrix with NCOL rows, each identical to vector."
  (let ((nrow (length vector)))
    (aprog1 (make-array (list nrow ncol)
                        :element-type (array-element-type vector))
      (dotimes (row-index nrow)
        (let ((element (aref vector row-index)))
          (dotimes (col-index ncol)
            (setf (aref it row-index col-index) element)))))))


(defun amap (element-type function &rest arrays)
  "Map arrays elementwise."
  (assert arrays)
  (let* ((arrays (mapcar (lambda (a)
                           (if (arrayp a)
                               a
                               (as-array a)))
                         arrays))
         (dimensions (array-dimensions (car arrays)))
         (result (make-array dimensions :element-type element-type)))
    (assert (every (lambda (a) (equal dimensions (array-dimensions a)))
                   (cdr arrays)))
    (apply #'map-into (flatten-array result) function
           (mapcar #'flatten-array arrays))
    result))

(declaim (inline zero-array))
(defun zero-array (dimensions &optional (element-type t))
  "An array filled with 0's (coerced to the given ELEMENT-TYPE)."
  (make-array dimensions :element-type element-type
                         :initial-element (coerce 0 element-type)))

(defgeneric column-sums (matrix &optional element-type)
  (:method ((matrix array) &optional element-type)
    (unlessf element-type (elementwise-float-contagion matrix))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (result (zero-array ncol)))
      (dotimes (row-index nrow)
        (dotimes (col-index ncol)
          (incf (aref result col-index) (aref matrix row-index col-index))))
      result)))

(defgeneric column-means (matrix &optional element-type)
  (:method ((matrix array) &optional element-type)
    (unlessf element-type (elementwise-float-contagion matrix))
    (let ((nrow (array-dimension matrix 0))
          (result (column-sums matrix element-type)))
      (map-into result (lambda (x) (/ x nrow)) result))))

(defgeneric row-sums (matrix &optional element-type)
  (:method ((matrix array) &optional element-type)
    (unlessf element-type (elementwise-float-contagion matrix))
    (let+ (((nrow ncol) (array-dimensions matrix))
           (result (zero-array nrow element-type)))
      (dotimes (row-index nrow)
        (dotimes (col-index ncol)
          (incf (aref result row-index) (aref matrix row-index col-index))))
      result)))

(defgeneric row-means (matrix &optional element-type)
  (:method ((matrix array) &optional element-type)
    (unlessf element-type (elementwise-float-contagion matrix))
    (let ((ncol (array-dimension matrix 1))
          (result (row-sums matrix element-type)))
      (map-into result (lambda (x) (/ x ncol)) result))))
