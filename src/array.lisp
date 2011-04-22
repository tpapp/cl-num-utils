;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric as-array (object &key copy?)
  (:documentation "Return OBJECT as an array.  May share structure.")
  (:method ((array array) &key copy?)
    (maybe-copy-array array copy?)))

;;; !! ROWS and COLUMNS could be speeded up considerably for Lisp arrays

(defgeneric rows (object &key copy?)
  (:documentation "Return the rows of a matrix-like OBJECT as a vector.  May
  share structure unless COPY?.")
  (:method ((matrix array) &key copy?)
    (iter
      (for row-index :below (nrow matrix))
      (collecting (if copy?
                      (sub matrix row-index t)
                      (displace-subarray matrix row-index))
                  :result-type vector)))
  (:method (object &key copy?)
    (rows (as-array object) :copy? copy?)))

(defgeneric columns (matrix &key copy?)
  (:documentation "Return the columns of a matrix-like object as a vector of
  vectors.  May share structure unless COPY?.")
  (:method ((matrix array) &key copy?)
    (declare (ignore copy?))
    (iter
      (for column-index :below (ncol matrix))
      (collecting (sub matrix t column-index)
                  :result-type vector)))
  (:method (object &key copy?)
    (columns (as-array object) :copy? copy?)))

(defgeneric map-columns (function matrix)
  (:documentation "Map columns of MATRIX using function.  FUNCTION is
  called with columns that are extracted as a vector, and the returned
  values are assembled into another matrix.  Element types and number
  of rows are established after the first function call, and are
  checked for conformity after that.  If function doesn't return a
  vector, the values are collected in a vector instead of a matrix."))

(defmethod map-columns (function (matrix array))
  (bind ((matrix (if (vectorp matrix)
                     (reshape matrix '(1 t) :row-major :copy? nil)
                     matrix))
         ((nil ncol) (array-dimensions matrix))
         result
         result-nrow)
    (iter
      (for col :from 0 :below ncol)
      (let ((mapped-col (funcall function (sub matrix t col))))
        (when (first-iteration-p)
          (if (vectorp mapped-col)
              (setf result-nrow (length mapped-col)
                    result (make-array (list result-nrow ncol)
                                       :element-type
                                       (array-element-type mapped-col)))
              (setf result (make-array ncol))))
        (if result-nrow
            (setf (sub result t col) mapped-col)
            (setf (aref result col) mapped-col))))
    result))

(defgeneric map-rows (function matrix)
  (:documentation "Map matrix row-wise into another matrix or vector, depending
 on the element type returned by FUNCTION."))

(defmethod map-rows (function (matrix array))
  (bind ((matrix (if (vectorp matrix)
                     (reshape matrix '(t 1) :row-major :copy? nil)
                     matrix))
         ((nrow nil) (array-dimensions matrix))
         result
         result-ncol)
    (iter
      (for row :from 0 :below nrow)
      (let ((mapped-row (funcall function (sub matrix row t))))
        (when (first-iteration-p)
          (if (vectorp mapped-row)
              (setf result-ncol (length mapped-row)
                    result (make-array (list nrow result-ncol)
                                       :element-type
                                       (array-element-type mapped-row)))
              (setf result (make-array nrow))))
        (if result-ncol
            (setf (sub result row t) mapped-row)
            (setf (aref result row) mapped-row))))
    result))

(defgeneric transpose (object)
  (:documentation "Transpose a matrix.")) 

(defmethod transpose ((matrix array))
  ;; transpose a matrix
  (bind (((nrow ncol) (array-dimensions matrix))
         (result (make-array (list ncol nrow)
                             :element-type (array-element-type matrix)))
         (result-index 0))
    (dotimes (col ncol)
      (dotimes (row nrow)
        (setf (row-major-aref result result-index) (aref matrix row col))
        (incf result-index)))
    result))

(defgeneric create (type element-type &rest dimensions)
  (:documentation "Create an object of TYPE with given DIMENSIONS and
  ELEMENT-TYPE (or a supertype thereof)."))

(defmethod create ((type (eql 'array)) element-type &rest dimensions)
  (make-array dimensions :element-type element-type))

(defmethod collect-rows (nrow function &optional (type 'array))
  (bind (result
         ncol)
    (iter
      (for row :from 0 :below nrow)
      (let ((result-row (funcall function)))
        (when (first-iteration-p)
          (setf ncol (length result-row)
                result (create type (array-element-type result-row) nrow ncol)))
        (setf (sub result row t) result-row)))
    result))

(defun collect-vector (n function &optional (element-type t))
  (bind (result)
    (iter
      (for index :from 0 :below n)
      (let ((element (funcall function)))
        (when (first-iteration-p)
          (setf result (make-array n :element-type element-type)))
        (setf (aref result index) element)))
    result))

(defun reshape-calculate-dimensions (dimensions size &optional list?)
  "If a single T is found among dimensions (a sequence), replace it with a
positive integer so that the product equals SIZE.  Otherwise check that the
product equals size.  Return a SIMPLE-FIXNUM-VECTOR, unless LIST?, in which case
it will return a list.  If dimensions is a single element, it is interpreted as
a sequence of length 1."
  (let* (missing-position
         (product 1)
         (position 0)
         (dimensions
          (map 'simple-fixnum-vector
               (lambda (dimension)
                 (aprog1
                     (cond
                       ((and (typep dimension 'fixnum) (<= 0 dimension))
                        (multf product dimension)
                        dimension)
                       ((eq dimension t)
                        (if missing-position
                            (error "Can't have more than one missing dimension.")
                            (progn (setf missing-position position) 0)))
                       (t (error "Can't interpret ~A as a dimension." dimension)))
                   (incf position)))
               (if (typep dimensions 'sequence) dimensions (vector dimensions)))))
    (if missing-position
        (setf (aref dimensions missing-position)
              (cond ((zerop size) 0)
                    ((zerop product) (error "Can't create a positive size ~
                                              with a zero dimension."))
                    (t (bind (((:values fraction remainder)
                               (floor size product)))
                         (assert (zerop remainder) ()
                                 "Substitution does not result in an integer.")
                         fraction))))
        (assert (= size product) () "Product of dimensions doesn't match size."))
    (if list?
        (coerce dimensions 'list)
        dimensions)))

(defun as-row (vector &key copy?)
  "Return vector as a matrix with one row."
  (check-type vector vector)
  (maybe-copy-array (displace-array vector (list 1 (length vector))) copy?))

(defun as-column (vector &key copy?)
  "Return vector as a matrix with one column."
  (check-type vector vector)
  (maybe-copy-array (displace-array vector (list (length vector) 1)) copy?))

(defgeneric reshape (object dimensions order &key copy? &allow-other-keys)
  (:documentation "Rearrange elements of an array-like object to new dimensions.
Order is :ROW-MAJOR or :COLUMN-MAJOR, the object will be treated as if it was
row- or column-major (but of course it does not have to be).  Unless COPY?, it
may share structure with the original.  Dimensions may can be a sequence, and
contain a single T, which is replaced to match sizes."))

(defmethod reshape ((array array) dimensions (order (eql :row-major)) &key copy?)
  (let* ((size (array-total-size array))
         (dimensions (reshape-calculate-dimensions dimensions size t)))
    (if copy?
        (aprog1 (make-similar-array array dimensions)
          (replace (displace-array it size) (displace-array array size)))
        (displace-array array dimensions))))

(defmethod reshape ((array array) dimensions (order (eql :column-major))
                    &key copy?)
  (declare (ignore copy?))
  (let* ((size (array-total-size array))
         (dimensions (reshape-calculate-dimensions dimensions size))
         (result (make-similar-array array (coerce dimensions 'list))))
    (with-indexing* ((array-dimensions array) array-index array-next :column-major? t)
      (with-indexing* (dimensions result-index result-next :column-major? t)
        (loop 
          repeat size
          do (progn (setf (row-major-aref result result-index)
                          (row-major-aref array array-index))
                    (array-next)
                    (result-next)))))
    result))

(defgeneric pref (object &rest indexes)
  (:documentation "Return a vector, with elements from OBJECT, extracted using
  INDEXES in parallel."))

(defmethod pref ((array array) &rest indexes)
  (let ((rank (array-rank array))
        (element-type (array-element-type array)))
    (assert (= rank (length indexes)))
    (when (zerop rank)
      (return-from pref (make-array 0 :element-type element-type)))
    (let* ((length (length (first indexes)))
           (result (make-array length :element-type element-type)))
      (assert (every (lambda (index) (= (length index) length)) (cdr indexes)))
      (loop
        :for element-index :below length
        :do (setf (aref result element-index)
                  (apply #'aref array
                                (mapcar (lambda (index) (aref index element-index))
                                        indexes))))
      result)))

(defgeneric filter-rows (predicate object)
  (:documentation "Filter rows of a matrix, with predicate applied to each row 
as vectors (which should not be modified).")
  (:method (predicate (object array))
    (sub object (which-rows predicate object) t)))

(defmacro with-filter-rows (matrix (&rest name-column-pairs) &body body)
  "Use BODY to filter rows of MATRIX, binding NAMEs to the given COLUMNs.

Example:
 (with-filter-rows #2A((0 1)
                       (101 80)
                       (203 200))
     ((a 0)
      (b 1))
     (and (oddp a) (< 100 b)))    ; => #2A((203 200))"
  (with-unique-names (vector)
    (let ((name-var-values (mapcar (lambda (name-column-pair)
                                     (bind (((name column) name-column-pair))
                                       (check-type name symbol)
                                       (list name
                                             (gensym (symbol-name name))
                                             column)))
                                   name-column-pairs)))
      `(let ,(mapcar #'cdr name-var-values)
         (filter-rows (lambda (,vector)
                        (let ,(mapcar (lambda (name-var-value)
                                        (bind (((name var nil) name-var-value))
                                          `(,name (aref ,vector ,var))))
                               name-var-values)
                          ,@body))
                      ,matrix)))))

;;;; dot product

(defgeneric dot (a b)
  (:documentation "Dot product."))

(defun sum-of-squares% (vector)
  (reduce #'+ vector :key (lambda (x) (* x (conjugate x)))))

(defmethod dot ((a vector) (b (eql t)))
  (sum-of-squares% a))

(defmethod dot ((a (eql t)) (b vector))
  (sum-of-squares% b))

(defmethod dot ((a vector) (b vector))
  (check-types (a b) vector)
  (let ((n (length a)))
    (assert (= n (length b)))
    (iter
      (for a-elt :in-vector a)
      (for b-elt :in-vector b)
      (summing (* (conjugate a-elt) b-elt)))))

;;; outer product

(defun outer (a b &key (function #'*) (element-type t))
  "Generalized outer product of A and B, using FUNCTION.  If either one is T, it is
replaced by the other one.  ELEMENT-TYPE can be used to give the element type."
  (cond
    ((and (eq t a) (eq t b)) (error "A and B can't both be T!"))
    ((eq t a) (setf a b))
    ((eq t b) (setf b a)))
  (check-types (a b) vector)
  (let* ((a-length (length a))
         (b-length (length b))
         (result (make-array (list a-length b-length) :element-type element-type))
         (result-index 0))
    (iter
      (for a-element :in-vector a)
      (iter
        (for b-element :in-vector b)
        (setf (row-major-aref result result-index)
              (funcall function a-element b-element))
        (incf result-index)))
    result))

;;; norms

;;; !! matrix norms would be nice, in that case we need to make these generic
;;; !! functions.

(defun norm1 (a)
  (reduce #'+ a :key #'abs))

(defun norm2 (a)
  "L2 norm."
  (sqrt (sum-of-squares% a)))

(defun normsup (a)
  (reduce #'max a :key #'abs))
