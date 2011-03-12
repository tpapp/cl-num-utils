;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Generic functions for querying dimensions and element types.  Only DIMS and
;;; ELEMENT-TYPE need to be defined, the rest fall back to these.

(defgeneric dims (object)
  (:documentation "Return the dimensions of object as a list.")
  (:method ((array array))
    (array-dimensions array)))

(defgeneric element-type (object)
  (:documentation "Return the element type of object.  Accessors return and
  allow setting subtypes of this type."))

;;; 

(defgeneric dim (object axis-number)
  (:documentation "Return the dimension of object along axis-number.")
  (:method (object axis-number)
    (aref (dims object) axis-number))
  (:method ((array array) axis-number)
    (array-dimension array axis-number)))

(defgeneric rank (object)
  (:documentation "Return the number of dimensions.")
  (:method (object)
    (length (dims object)))
  (:method ((array array))
    (array-rank array)))

(defgeneric nrow (object)
  (:documentation "Return number of rows in object.  Signal an error if OBJECT
  doesn't have exactly two dimensions.")
  (:method (object)
    (bind (((nrow nil) (dims object)))
      nrow))
  (:method ((array array))
    (assert (= 2 (array-rank array)) () "Array is not a matrix.")
    (array-dimension array 0)))

(defgeneric ncol (object)
  (:documentation "Return number of columns in object.  Signal an error if
  OBJECT doesn't have exactly two dimensions.")
  (:method (object)
    (bind (((nil ncol) (dims object)))
      ncol))
  (:method ((array array))
    (assert (= 2 (array-rank array)) () "Array is not a matrix.")
    (array-dimension array 1)))

;;; SUB -- extracting subsets of objects with rectilinear indexing

(defgeneric sub (object &rest index-specifications)
  (:documentation "Return a subset of object for the given index
  specifications, one for each dimension.

  Index specifications have the following syntax (also see general notes below):

  fixnum - Addressing a single element, also, the dimension is dropped, ie rank
    decreases by 1.

  bit-vector - Addressing elements where the bit-vector is equal to 1.

  vector of fixnums - Elements at those coordinates.

  (si start end &optional by) - Sequence indexing, from START by BY, not
    including END.

  (rev index-specification) - Reverse index specification.

  (cat index-specification ...) - Concatenation of index specification.  FIXNUMs
    are treated as vectors of a single element.

  All fixnums are allowed to be negative, in which case they are counted from
  the end.  If END is 0, it includes the last element.  Because of these
  syntactic conveniences, resolution of index specifications may be delayed
  until the dimensions are known.

  See WITH-INDEXING for implementation details."))

(defgeneric (setf sub) (source target &rest index-specifications)
  (:documentation "Set the subset of TARGET (according to INDEX-SPECIFICATIONS)
  in SOURCE.  See SUB for the documentation on the syntax of index
  specifications."))

(define-condition sub-incompatible-dimensions (error)
  ()
  (:documentation "Rank or dimensions are incompatible."))

(define-condition sub-invalid-array-index (error)
  ((index :accessor index :initarg :index)
   (dimension :accessor dimension :initarg :dimension)))

(define-condition sub-invalid-index-specification (error)
  ((index-specification :accessor index-specification
                        :initarg :index-specification)))

(defstruct (si (:constructor si (start end &optional by strict-direction?)))
  "Sequence indexing: addresses elements from START to END, by BY.  Resolution
may be delayed until dimension is known, so any sign of BY can be used.  Unless
STRICT-DIRECTION?, the sign of BY is auto-adjusted at the time of resolution."
  (start 0 :type fixnum)
  (end 0 :type (or fixnum null))
  (by 1 :type fixnum)
  (strict-direction? nil :type boolean))

(defstruct (resolved-si (:constructor resolved-si (start length by)))
  "Resolved range indexing."
  (start 0 :type fixnum)
  (length 0 :type fixnum)
  (by 1 :type fixnum))

(defstruct (delayed-index-specification
             (:constructor delayed-index-specification (type data)))
  "A index-specification with relayed resolution."
  type data)

(defun cat (&rest index-specifications)
  "Concatenation of index-specifications."
  (if (cdr index-specifications)
      (if (every #'vectorp index-specifications)
          (apply #'concatenate 'simple-fixnum-vector index-specifications)
          (delayed-index-specification 'concatenation index-specifications))
      (car index-specifications)))

(defun rev (index-specification)
  "Reverse of index-specification."
  (typecase index-specification
    (fixnum index-specification)
    (vector (reverse index-specification))
    (otherwise (delayed-index-specification 'reverse index-specification))))

(defmethod sub ((si si) &rest index-specifications)
  (bind (((index-specification) index-specifications))
    (delayed-index-specification 'sub (cons si index-specification))))

(defmethod sub ((di delayed-index-specification) &rest index-specifications)
  (bind (((index-specification) index-specifications))
    (delayed-index-specification 'sub (cons di index-specification))))

(defun resolve-t (dimension)
  "Resolve a T index specification."
  (resolved-si 0 dimension 1))

(defun maybe-resolved-si (start length by force-vector?)
  "When FORCE-VECTOR?, return a SIMPLE-FIXNUM-VECTOR, otherwise a RESOLVED-SI."
  (if force-vector?
      (let ((vector (make-array length :element-type 'fixnum))
            (index start))
        (dotimes (vector-index length vector)
          (setf (aref vector vector-index) index)
          (incf index by)))
      (resolved-si start length by)))

(defun resolve-index-specification (index-specification dimension
                                    &optional force-vector?)
  "Resolve delayed operations in INDEX-SPECIFICATION given the dimension.
Return either a FIXNUM, a RESOLVED-SI object, or a SIMPLE-FIXNUM-VECTOR.  When
FORCE-VECTOR?, a result that would be RESOLVED-SI is converted into a vector."
  (declare (optimize debug))
  (when dimension
    (check-type dimension (integer 0 #.most-positive-fixnum)))
  (bind (((:flet resolve-index (index &optional end?))
          ;; Resolve an atomic index.  If END?, 0 and NIL stand for the dimension.
          (check-type index (or fixnum null))
           (cond
             ((or (null index) (zerop index))
              (if end?
                  (progn
                    (assert dimension () 
                            "Can't resolve 0 at the end without a dimension.")
                    dimension)
                  (progn
                    (assert index ()
                            "NIL only has a meaning as an end delimiter.")
                    0)))
             ((minusp index)
              (assert dimension () 
                      "Can't resolve a negative index without a dimension.")
              (aprog1 (+ dimension index)
                (assert (<= 0 it) () 'sub-invalid-array-index
                        :index index :dimension dimension)))
             (t index))))
    (etypecase index-specification
      ((eql t) (resolve-t dimension))
      (fixnum (resolve-index index-specification))
      (bit-vector (progn
                    (assert (= dimension (length index-specification)))
                    (positions index-specification)))
      (vector (map 'simple-fixnum-vector #'resolve-index index-specification))
      (si (bind (((:slots-r/o start end by strict-direction?) index-specification)
                 (start (resolve-index start))
                 (end (resolve-index end t))
                 (span (- end start)))
            (if strict-direction?
                (assert (plusp (* span by)) ()
                        "Invalid indexing ~A->~A by ~A." start end by)
                (setf by (* (signum span) (abs by))))
            (maybe-resolved-si start (ceiling span by) by force-vector?)))
      (delayed-index-specification
         (bind (((:slots-r/o type data) index-specification))
           (ecase type
             (concatenation
                (apply #'concatenate 'simple-fixnum-vector
                       (mapcar (lambda (index-specification)
                                 (aetypecase (resolve-index-specification
                                              index-specification dimension t)
                                   (fixnum (vector it))
                                   (simple-fixnum-vector it)))
                               data)))
             (reverse
                (let ((index-specification
                       (resolve-index-specification data dimension)))
                  (etypecase index-specification
                    (fixnum index-specification)
                    (simple-fixnum-vector (reverse index-specification))
                    (resolved-si
                       (bind (((:slots-r/o start length by) index-specification))
                         (maybe-resolved-si (+ start (* (1- length) by))
                                            length (- by) force-vector?))))))
             (sub (sub (resolve-index-specification (car data) dimension t)
                       (cdr data)))))))))

(defun resolve-index-specifications (index-specifications dimensions)
  "Resolve multiple index-specifications."
  (map 'vector #'resolve-index-specification index-specifications dimensions))

(defun row-major-coefficients (dimensions)
  "Calculate coefficients for row-major mapping."
  (let* ((cumprod 1)
         (rank (length dimensions))
         (coefficients (make-array rank :element-type 'fixnum)))
    (iter
      (for axis-number :from (1- rank) :downto 0)
      (setf (aref coefficients axis-number) cumprod
            cumprod (* cumprod (aref dimensions axis-number))))
    coefficients))

(defun drop-dimensions (index-specifications coefficients)
  "Drop single dimensions.  Return (values OFFSET NEW-INDEX-SPECIFICATIONS
NEW-COEFFICIENTS)."
  (iter
    (with offset := 0)
    (for index-specification :in-vector index-specifications)
    (for coefficient :in-vector coefficients)
    (if (numberp index-specification)
        (incf offset (* index-specification coefficient))
        (progn
          (collect index-specification :into new-index-specifications
                   :result-type vector)
          (collect coefficient :into new-coefficients
                   :result-type simple-fixnum-vector)))
    (finally
     (return (values offset
                     new-index-specifications
                     new-coefficients)))))

(defun index-specification-dimension (index-specification)
  "Dimension of a index-specification."
  (etypecase index-specification
    (fixnum 1)
    (vector (length index-specification))
    (resolved-si (resolved-si-length index-specification))))

(defun index-specification-dimensions (index-specifications)
  "Dimensions of index-specifications."
  (map 'simple-fixnum-vector #'index-specification-dimension
       index-specifications))

(defun map-counter (index-specification counter)
  "Map an index (starting from zero) to an index within a index-specification.
No validity checks, this function is meant for internal use and always expects a
valid index."
  (etypecase index-specification
    (fixnum index-specification)
    (resolved-si (+ (resolved-si-start index-specification)
                    (* counter (resolved-si-by index-specification))))
    (vector (aref index-specification counter))))

(defun increment-index-counters (counters dimensions)
  "Increment index counters, beginning from the end.  Return the index
of the last one that was changed.  The second value is T when the
first index has reached its limit, ie the array has been walked and
all the counters are zero again."
  (iter
    (for axis-number :from (1- (length dimensions)) :downto 0)
    (if (= (incf (aref counters axis-number))
           (aref dimensions axis-number))
        (setf (aref counters axis-number) 0)
        (return-from increment-index-counters axis-number)))
  (values 0 t))

(defun map-counters (offset index-specifications coefficients counters cumsums
                     valid-end)
  "Recalculate cumsums, return flat index."
  (let ((cumsum (if (zerop valid-end)
                    offset
                    (aref cumsums (1- valid-end)))))
    (iter
      (for counter :in-vector counters :from valid-end
           :with-index axis-number)
      (for index-specification :in-vector index-specifications :from valid-end)
      (for coefficient :in-vector coefficients :from valid-end)
      (incf cumsum (* coefficient (map-counter index-specification counter)))
      (setf (aref cumsums axis-number) cumsum))
    cumsum))

(defmacro with-indexing ((index-specifications dimensions index next &key
                         (effective-dimensions (gensym "EFFECTIVE-DIMENSIONS"))
                         (counters (gensym "COUNTERS")))
                         &body body)
  "Establish incrementation and index-calculation functions within BODY.  The
sequence INDEX-SPECIFICATIONS constains the index specifications, and DIMENSIONS
contains the dimensions of the object indexed.  The current index is bound to
INDEX, which you can step with NEXT.  When NEXT returns non-nil, you have
reached the last valid index.  EFFECTIVE-DIMENSIONS is a vector of fixnums that
contains the effective dimensions traversed (may be shorted than DIMENSIONS, or
have length zero, if dimensions are dropped).  It may be used to check for
termination by calculating its product (the number of elements traversed), but
the return value of NEXT is recommended.  COUNTERS gives access to counters.

The consequences are undefined if COUNTERS is modified.  See source for comments
on implementation details."
;;; WITH-INDEXING is the user interface of an iteration construct that walks the
;;; (indexes of the) elements on an array.  Indexing can be row- or
;;; column-major, or even represent axis permutations, etc, but order of
;;; traversal is contiguous only for row-major arrays.
;;;
;;; Here is how it works:
;;;
;;; 0. RESOLVE-INDEX-SPECIFICATION(S) resolves index specifications to one of
;;;    the following: a FIXNUM, a SIMPLE-FIXNUM-VECTOR, or RESOLVED-SI.
;;;
;;; 1. An affine mapping is established, which is the sum of indexes multiplied
;;;    by corresponding coefficients.  This is general enough to permit row- and
;;;    colum-major mappings, or even axis permutations.
;;;
;;; 2. Dropped dimensions (denoted by a single integer) are removed, and the
;;;    corresponding partial sum is added as an offset.
;;;
;;; 3. An index counter (a vector of fixnums) is initialized with zeros, and
;;;    incremented with each step.  The set of indices changed is kept track of.
;;;    The sum of coefficients is calculated, using partial sums from previous
;;;    iterations to the extent it is possible.
  (check-type index symbol)
  (check-type next symbol)
  (once-only (dimensions index-specifications)
    (with-unique-names (coefficients offset rank cumsums valid-end map-counters%)
      `(bind ((,dimensions (as-simple-fixnum-vector ,dimensions))
              (,rank (length ,index-specifications)))
         (assert (= ,rank (length ,dimensions)) () 'sub-incompatible-dimensions)
         (bind ((,index-specifications (resolve-index-specifications
                                        ,index-specifications ,dimensions))
                (,coefficients (row-major-coefficients ,dimensions))
                ((:values ,offset ,index-specifications ,coefficients)
                 (drop-dimensions ,index-specifications ,coefficients))
                (,effective-dimensions (index-specification-dimensions
                                        ,index-specifications))
                (,counters (make-array ,rank :element-type 'fixnum
                                       :initial-element 0))
                (,cumsums (make-array ,rank :element-type 'fixnum))
                (,valid-end 0)
                ((:flet ,map-counters% ())
                 (map-counters ,offset ,index-specifications ,coefficients
                               ,counters ,cumsums ,valid-end))
                (,index (,map-counters%))
                ((:flet ,next ())
                 (bind (((:values valid-end end?)
                         (increment-index-counters ,counters ,effective-dimensions)))
                   (setf ,valid-end valid-end
                         ,index (,map-counters%))
                   end?)))
           ;; !!! dynamic extent & type declarations
           ;; !!! check optimizations
           ,@body)))))

(defun map-counters* (coefficients counters cumsums valid-end)
  "Recalculate cumsums, return flat index.  No offset, no index specification,
counters map to themselves."
  (let ((cumsum (if (zerop valid-end) 0 (aref cumsums (1- valid-end)))))
    (iter
      (for counter :in-vector counters :from valid-end
           :with-index axis-number)
      (for coefficient :in-vector coefficients :from valid-end)
      (incf cumsum (* coefficient counter))
      (setf (aref cumsums axis-number) cumsum))
    cumsum))

(defmacro with-indexing* ((dimensions index next &key
                                      column-major?
                                      (counters (gensym "COUNTERS")))
                          &body body)
  "A simpler version of WITH-INDEXING, with all index-specifications as T.  
COLUMN-MAJOR? uses column-major indexing."
  ;; Implementation note: for column-major traversal, we simply reverse both the
  ;; coefficients and the dimensions.
  (check-type index symbol)
  (check-type next symbol)
  (once-only (dimensions column-major?)
    (with-unique-names (coefficients rank cumsums valid-end)
      `(bind (((:flet nreverse-if-cm (vector))
               (if ,column-major?
                   (nreverse vector)
                   vector))
              (,dimensions (as-simple-fixnum-vector ,dimensions t))
              (,coefficients (nreverse-if-cm (row-major-coefficients ,dimensions)))
              (,dimensions (nreverse-if-cm ,dimensions))
              (,rank (length ,dimensions))
              (,counters (make-array ,rank :element-type 'fixnum
                                     :initial-element 0))
              (,cumsums (make-array ,rank :element-type 'fixnum))
              (,valid-end 0)
              (,index 0)
              ((:flet ,next ())
               (bind (((:values valid-end end?)
                       (increment-index-counters ,counters ,dimensions)))
                 (setf ,valid-end valid-end
                       ,index
                       (map-counters* ,coefficients ,counters ,cumsums 
                                      ,valid-end))
                 end?)))
         (declare (ignorable (function ,next)))
         ,@body))))

(defmethod sub ((array array) &rest index-specifications)
  (with-indexing (index-specifications (array-dimensions array) index
                                       next-index
                                       :effective-dimensions dimensions)
    (if (zerop (length dimensions))
        (row-major-aref array index)
        (let ((result (make-array (coerce dimensions 'list)
                                  :element-type
                                  (array-element-type array))))
          (iter
            (for result-index :from 0)
            (setf (row-major-aref result result-index)
                  (row-major-aref array index))
            (until (next-index)))
          result))))

(defmethod sub ((list list) &rest index-specifications)
  (with-indexing (index-specifications (vector (length list)) index next-index
                                       :effective-dimensions dimensions)
    (if (zerop (length dimensions))
        (nth index list)
        ;; not very efficient, but lists are not ideal for random access
        (iter
          (collecting (nth index list))
          (until (next-index))))))

;;; (setf sub) with array target

(defmethod (setf sub) (source (target array) &rest index-specifications)
  (with-indexing (index-specifications (array-dimensions target) index next-index
                               :effective-dimensions dimensions)
    (iter
      (setf (row-major-aref target index) source)
      (until (next-index))))
  source)

(defmethod (setf sub) ((source array) (target array) &rest index-specifications)
  (with-indexing (index-specifications (array-dimensions target) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (coerce (array-dimensions source) 'vector))
            () 'sub-incompatible-dimensions)
    (iter
      (for source-index :from 0)
      (setf (row-major-aref target index)
            (row-major-aref source source-index))
      (until (next-index))))
  source)

(defmethod (setf sub) ((source list) (target array) &rest index-specifications)
  (with-indexing (index-specifications (array-dimensions target) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (vector (length source)))
            () 'sub-incompatible-dimensions)
    (iter
      (for element :in source)
      (setf (row-major-aref target index) element)
      (until (next-index))))
  source)

;;; (setf sub) with list target

(defmethod (setf sub) (source (list list) &rest index-specifications)
  (with-indexing (index-specifications (vector (length list)) index next-index)
    (iter
      (setf (nth index list) source)
      (until (next-index)))))

(defmethod (setf sub) ((source list) (list list) &rest index-specifications)
  (with-indexing (index-specifications (vector (length list)) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (vector (length source))) ()
            'sub-incompatible-dimensions)
    (iter
      (for element :in source)
      (setf (nth index list) element)
      (until (next-index)))))

(defmethod (setf sub) ((source vector) (list list) &rest index-specifications)
  (with-indexing (index-specifications (vector (length list)) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (vector (length source))) ()
            'sub-incompatible-dimensions)
    (iter
      (for element :in-vector source)
      (setf (nth index list) element)
      (until (next-index)))))
