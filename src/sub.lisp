;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric sub (object &rest selections)
  (:documentation "Return a subset of object using the Cartesian product of
  the given index selections, one for each dimension.

  Available standard selections:

    fixnum - select that coordinate, drop a dimension.
  
    vector of fixnums - select those coordinates.
  
    (cons start end) - coordinates i such that start <= i < end.
  
    (seq start end &optional by strict-direction?) - arithmetic sequence, the
      sign of BY is adjusted unless STRICT-DIRECTION.
  
    (cat &rest ...) - concatenate selections
  
    (rev ...) - reverse the order of elements
  
    (sub ... sub-selection) - select indexes using selection

  In all of the above, negative numbers count back from DIMENSION.  Because of
  this, resolution may be delayed until SUB is invoked, this is handled
  transparently."))

(defgeneric (setf sub) (source target &rest selections)
  (:documentation "Set the subset of TARGET (according to SELECTIONS) in
  SOURCE.  See SUB for the documentation on the syntax of index selections."))

(define-condition sub-incompatible-dimensions (error)
  ()
  (:documentation "Rank or dimensions are incompatible."))

(define-condition sub-invalid-array-index (error)
  ((index :accessor index :initarg :index)
   (dimension :accessor dimension :initarg :dimension))
  (:documentation "Resolved index is not in [0,dimension)."))

;;; resolution of selections

(defstruct (delayed-cat (:constructor cat (&rest selections)))
  "Delayed concatenation."
  (selections nil :type list))

(defstruct (delayed-rev (:constructor delayed-rev (selection)))
  "Delayed reverse."
  (selection))

(defstruct (delayed-sub (:constructor delayed-sub (selection sub-selection)))
  "Delayed selection."
  selection sub-selection)

(defmethod sub ((object delayed-sub) &rest selections)
  (let+ (((sub-selection) selections))
    (delayed-sub object sub-selection)))

(defmethod sub ((object delayed-rev) &rest selections)
  (let+ (((sub-selection) selections))
    (delayed-sub object sub-selection)))

(defmethod sub ((object delayed-cat) &rest selections)
  (let+ (((sub-selection) selections))
    (delayed-sub object sub-selection)))

(defstruct (delayed-ivec
             (:constructor delayed-ivec (start end by strict-direction?)))
  "Delayed index sequence evaluation."
  (start nil :type fixnum)
  (end nil :type (or fixnum null))
  (by nil :type fixnum)
  (strict-direction? nil :type boolean))

(defun ivec* (start-or-end &optional (end 0 end?) (by 1) strict-direction?)
  "Selection based on an arithmetic sequence -- see IVEC for the arguments.
  Used in calls to SUB, allowsnegative or NIL indexes."
  (let+ (((&values start end) (if end?
                                  (values start-or-end end)
                                  (values 0 start-or-end))))
    (delayed-ivec start end by strict-direction?)))

(defgeneric rev (selection)
  (:documentation "Reverse the order of elements.")
  (:method (selection)
    (delayed-rev selection))
  (:method ((vector vector))
    (reverse vector)))

(defgeneric sub-resolve-index (index dimension)
  (:documentation "Resolve a single index, always returning a FIXNUM >= 0 and
  < dimension.  The default behavior is the following: possitive numbers are
  resolved to themselves (checked to be lower than dimension), negative
  numbers are resolved to dimension-index, and NIL is resolved to DIMENSION.")
  (:method ((index null) dimension)
    dimension)
  (:method ((index fixnum) dimension)
      (if (minusp index)
          (aprog1 (+ dimension index)
            (assert (<= 0 it)))
          (aprog1 index
            (assert (< it dimension))))))

(defun sub-resolve-end-index (index dimension)
  "Resolve an index which denotes an end index, and thus it is allowed to be
equal to DIMENSION."
  (if (and index (= index dimension))
      index
      (sub-resolve-index index dimension)))

(defgeneric sub-resolve-selection (selection dimension &optional expand?)
  (:documentation "Resolve selection to an object representing indexes to
  be walked.  FIXNUMs represent a single index that drops dimensions.  EXPAND?
  forces expansion to either a FIXNUM or a vector of fixnums.")
  ;; Implementation note: currently we resolve to one of the following:
  ;; FIXNUM, SIMPLE-FIXNUM-VECTOR and CONS.  Thus walkers only need to be
  ;; defined for these.
  (:method ((index fixnum) dimension &optional expand?)
    (declare (ignore expand?))
    (sub-resolve-index index dimension))
  (:method ((vector vector) dimension &optional expand?)
    (declare (ignore expand?))
    (map 'simple-fixnum-vector
         (lambda (i) (sub-resolve-index i dimension))
         vector))
  (:method ((range cons) dimension &optional expand?)
    (let+ (((start . end) range)
           (start (sub-resolve-index start dimension))
           (end (sub-resolve-end-index end dimension)))
      (assert (< start end))
      (if expand?
          (ivec start end)
          (cons start end))))
  (:method ((mask bit-vector) dimension &optional expand?)
    (declare (ignore expand?))
    (assert (= (length mask) dimension))
    (positions mask))
  (:method ((selection (eql t)) dimension &optional expand?)
    (if expand?
        (ivec 0 dimension)
        (cons 0 dimension)))
  (:method ((selection delayed-cat) dimension &optional expand?)
    (declare (ignore expand?))
    (concat* 'fixnum
             (mapcar (lambda (s) (sub-resolve-selection s dimension t))
                     (delayed-cat-selections selection))))
  (:method ((selection delayed-sub) dimension &optional expand?)
    (declare (ignore expand?))
    (let+ (((&structure-r/o delayed-sub- selection
                            sub-selection) selection))
      (sub (sub-resolve-selection selection dimension t) sub-selection)))
  (:method ((selection delayed-rev) dimension &optional expand?)
    (declare (ignore expand?))
    (reverse (sub-resolve-selection (delayed-rev-selection selection)
                                    dimension t)))
  (:method ((selection delayed-ivec) dimension &optional expand?)
    (declare (ignore expand?))
    (let+ (((&structure-r/o delayed-ivec- start end by strict-direction?)
            selection))
      (ivec (sub-resolve-index start dimension)
            (sub-resolve-end-index end dimension)
            by strict-direction?))))

;;; walking selections
;;; 
;;; Recursive traversal of selections is implemented by closures chained
;;; together.  Once the index reaches its last value, the next one is
;;; incremented.  The cumulative sum is only partially calculated.

(defgeneric sub-dimension (resolved-selection)
  (:documentation "Return the dimension of resolved selection.")
  (:method ((index fixnum)) 1)
  (:method ((vector vector)) (length vector))
  (:method ((range cons)) (- (cdr range) (car range))))

(defmacro walker-closure ((coefficient next-walker)
                          (&rest bindings)
                          declarations
                          index increment test reset)
  "Define closure for walking a selection.  BINDINGS go in front (inside
  LET+), should perform the initialization.  INDEX is used for the index,
  INCREMENT increments it (used for side effect), TEST tests for termination,
  while RESET restarts."
  (with-unique-names (cumulative-sum)
    (once-only (coefficient next-walker)
      `(let+ (,@bindings
              ,cumulative-sum)
         ,declarations
         (lambda ()
           (when ,test
             (setf ,cumulative-sum (funcall ,next-walker))
             ,reset)
           (unlessf ,cumulative-sum (funcall ,next-walker))
           (prog1 (+ (* ,coefficient ,index) ,cumulative-sum)
             ,increment))))))

(defgeneric sub-walker (resolved-selection coefficient next-walker)
  (:documentation "Return a walker for RESOLVED-SELECTION, chained to the next
  WALKER.")
  ;; note: fixnums don't have walkers, as they are not meant to be walked.
  (:method ((vector vector) coefficient next-walker)
    (walker-closure (coefficient next-walker)
                    ((length (length vector))
                     (index 0))
                    (declare (fixnum length index))
                    (aref vector index)
                    (incf index)
                    (= index length)
                    (setf index 0)))
  (:method ((range cons) coefficient next-walker)
    (walker-closure (coefficient next-walker)
                    (((start . end) range)
                     (index start))
                    (declare (fixnum start end index))
                    index
                    (incf index)
                    (= index end)
                    (setf index start))))

(defun row-major-coefficients (dimensions)
  "Calculate coefficients for row-major mapping."
  (let* ((dimensions (coerce dimensions 'simple-fixnum-vector))
         (cumprod 1)
         (rank (length dimensions))
         (coefficients (make-array rank :element-type 'fixnum)))
    (iter
      (for axis-number :from (1- rank) :downto 0)
      (setf (aref coefficients axis-number) cumprod
            cumprod (* cumprod (aref dimensions axis-number))))
    coefficients))

(define-condition object-walked (error)
  ()
  (:documentation "This condition is raised when the innermost walker is
called more than one time, ie all indices have been visited."))

(defun chained-walker (resolved-selections coefficients
                       &optional (offset 0))
  "Drop single dimensions (ie FIXNUMS) from resolved selections, and build
walker using COEFFICIENTS.  Return (values WALKER OFFSET)."
  (let+ ((end? nil)
         (walker (lambda ()
                   (when end?
                     (error 'object-walked))
                   (setf end? t)
                   offset)))
    (iter
      (for selection :in-vector resolved-selections)
      (for coefficient :in-vector coefficients)
      (if (fixnum? selection)
          (incf offset (* coefficient selection))
          (setf walker (sub-walker selection coefficient walker))))
    walker))

(defun effective-dimensions (resolved-selections)
  "Return the dimensions of the result as a list, dropping dimensions for
fixnum selections."
  (iter
    (for selection :in-vector resolved-selections)
    (unless (fixnum? selection)
      (collect (sub-dimension selection)))))

(defmacro with-indexing ((selections dimensions index next 
                          &key effective-dimensions)
                         &body body)
  "Establish incrementation and index-calculation functions within BODY.  The
sequence SELECTIONS constains the index selections, and DIMENSIONS contains
the dimensions of the object indexed.  These are resolved and walked.  

The current index is bound to INDEX, and is stepped with NEXT.  When NEXT
returns non-nil, the last valid index has been reached.

When given EFFECTIVE-DIMENSIONS will be assigned a list of fixnums is a vector
of fixnums that contains the effective dimensions traversed (may be shorted
than DIMENSIONS, or have length zero, if dimensions are dropped).  It may be
used to check for termination by calculating its product (the number of
elements traversed), but the return value of NEXT is recommended."
  (check-type index symbol)
  (check-type next symbol)
  (once-only (dimensions selections)
    (with-unique-names (walker)
      `(let+ ((,dimensions (as-simple-fixnum-vector ,dimensions)))
         (assert (= (length ,selections) (length ,dimensions)) ()
                 'sub-incompatible-dimensions)
         (let+ ((,selections
                 (map 'vector 
                      (lambda (s d) (sub-resolve-selection s d))
                      ,selections ,dimensions))
                (,walker (chained-walker ,selections
                                         (row-major-coefficients ,dimensions)))
                ,@(when effective-dimensions
                    `((,effective-dimensions
                       (effective-dimensions ,selections))))
                (,index (funcall ,walker))
                ((&flet ,next ()
                   (handler-case (prog1 nil
                                   (setf ,index (funcall ,walker)))
                     (object-walked () t)))))
           ;; !!! dynamic extent & type declarations
           ;; !!! check optimizations
           ,@body)))))

;;; sub for arrays

(defmethod sub ((array array) &rest selections)
  (with-indexing (selections (array-dimensions array) index
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

;;; sub for lists

(defmethod sub ((list list) &rest selections)
  (with-indexing (selections (vector (length list)) index next-index
                                   :effective-dimensions dimensions)
    (if (zerop (length dimensions))
        (nth index list)
        ;; not very efficient, but lists are not ideal for random access
        (iter
          (collecting (nth index list))
          (until (next-index))))))

;;; (setf sub) with array target

(defmethod (setf sub) (source (target array) &rest selections)
  (with-indexing (selections (array-dimensions target) index
                                       next-index)
    (iter
      (setf (row-major-aref target index) source)
      (until (next-index))))
  source)

(defmethod (setf sub) ((source array) (target array) &rest selections)
  (with-indexing (selections (array-dimensions target) index next-index
                                       :effective-dimensions dimensions)
    (assert (equal dimensions (array-dimensions source))
            () 'sub-incompatible-dimensions)
    (iter
      (for source-index :from 0)
      (setf (row-major-aref target index)
            (row-major-aref source source-index))
      (until (next-index))))
  source)

(defmethod (setf sub) ((source list) (target array) &rest selections)
  (with-indexing (selections (array-dimensions target) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (list (length source)))
            () 'sub-incompatible-dimensions)
    (iter
      (for element :in source)
      (setf (row-major-aref target index) element)
      (until (next-index))))
  source)

;;; (setf sub) with list target

(defmethod (setf sub) (source (list list) &rest selections)
  (with-indexing (selections (vector (length list)) index next-index)
    (iter
      (setf (nth index list) source)
      (until (next-index)))))

(defmethod (setf sub) ((source list) (list list) &rest selections)
  (with-indexing (selections (vector (length list)) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (list (length source))) ()
            'sub-incompatible-dimensions)
    (iter
      (for element :in source)
      (setf (nth index list) element)
      (until (next-index)))))

(defmethod (setf sub) ((source vector) (list list) &rest selections)
  (with-indexing (selections (vector (length list)) index next-index
                                       :effective-dimensions dimensions)
    (assert (equalp dimensions (list (length source))) ()
            'sub-incompatible-dimensions)
    (iter
      (for element :in-vector source)
      (setf (nth index list) element)
      (until (next-index)))))

;;; utility functions

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

(defun mask (predicate sequence)
  "Map sequence into a simple-bit-vector, using 1 when PREDICATE yields true,
  0 otherwise."
  (map 'simple-bit-vector (predicate-as-flag predicate) sequence))

(defun which (predicate sequence)
  "Return an index of the positions in SEQUENCE which satisfy PREDICATE."
  (let ((index 0)
        positions)
    (map nil (lambda (element)
               (when (funcall predicate element)
                 (push index positions))
               (incf index))
         sequence)
    (coerce (nreverse positions) 'simple-fixnum-vector)))
