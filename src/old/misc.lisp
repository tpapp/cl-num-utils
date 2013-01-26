;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun nonnegative? (x)
  "Returns T if x >= 0, otherwise NIL."
  (<= 0 x))

(defun nonpositive? (x)
  "Returns T if x <= 0, otherwise NIL."
  (>= 0 x))

(defun divides? (number divisor)
  "Test if DIVISOR divides NUMBER without remainder, and if so, return the
  quotient.  Works generally, but makes most sense for rationals."
  (let+ (((&values quot rem) (floor number divisor)))
    (when (zerop rem)
      quot)))

(defmacro nif (value positive negative &optional zero)
  "Numeric if."
  (once-only (value)
    `(cond
       ((plusp ,value) ,positive)
       ((minusp ,value) ,negative)
       ,@(when zero
           `((t ,zero))))))

(defmacro anif (value positive negative &optional zero)
  "Anaphoric numeric if."
  `(let ((it ,value))
     (cond
       ((plusp it) ,positive)
       ((minusp it) ,negative)
       ,@(when zero
           `((t ,zero))))))

(defun bic (a b)
  "Biconditional."
  (if a b (not b)))

(define-modify-macro multf (&rest values) * "Multiply by the arguments")

(defun as-integer (number)
  "If NUMBER represents an integer (as an integer, complex, or float,
etc), return it as an integer, otherwise signal an error."
  (declare (inline as-integer))
  (etypecase number
    (integer number)
    (complex
       (assert (zerop (imagpart number)) ()
               "~A has non-zero imaginary part." number)
       (as-integer (realpart number)))
    (t (let+ (((&values int frac) (floor number)))
         (assert (zerop frac) ()
                 "~A has non-zero fractional part." number)
         int))))

(defun common-supertype (type-1 type-2)
  "Return a common supertype of the two types.  Might not be the narrowest - it
defaults to T if neither type is a subtype of the other.  Intended use is
finding a common array element type."
  (cond
    ((subtypep type-1 type-2) type-2)
    ((subtypep type-2 type-1) type-1)
    (t t)))

(defun round* (number digits)
  "Round NUMBER to the given number of decimal digits."
  (let* ((pow10 (expt 10 (- digits)))
         (rounded-number (* (round number pow10) pow10)))
    (if (and (floatp number) (plusp digits))
        (float rounded-number number)
        rounded-number)))

(defun maybe-copy-array (array copy?)
  "If COPY?, return a shallow copy of array, otherwise the original.  Useful
for implementing the COPY? semantics of methods."
  (if copy?
      (copy-array array)
      array))

(defun convex-combination (a b alpha)
  "Convex combination (1-alpha)*a+alpha*b."
  (+ (* (- 1 alpha) a) (* alpha b)))

(defun vector-last (vector &optional (n 1))
  "Like LAST, but for vectors."
  (aref vector (- (length vector) n)))

(defun common (sequence &key (key #'identity) (test #'eql) failure error)
  "If the elements of sequence are the same (converted with KEY, compared with
TEST), return that, otherwise FAILURE.  When ERROR?, an error is signalled
instead.  The second value is true iff elements are the same."
  (values
   (reduce (lambda (a b)
             (if (funcall test a b)
                 a
                 (if error
                     (apply #'error (ensure-list error))
                     (return-from common failure))))
           sequence
           :key key)
   t))

(defun common-length (&rest sequences)
  "If sequences have the same length, return that, otherwise NIL."
  (common sequences :key #'length :test #'=))

(defun common-dimensions (&rest arrays)
  "If arrays have the same dimensions, return that, otherwise NIL."
  (common arrays :key #'array-dimensions :test #'equalp))

(defun format-number (number &key (int-digits 3) (exp-digits 1))
  "Format number nicely."
  (if (integerp number)
      (format nil "~d" number)
      (format nil "~,v,v,,g" int-digits exp-digits number)))

(defun ignore-error (function &key replacement-value)
  "Wrap function to return REPLACEMENT-VALUE in case of errors."
  ;; ?? maybe write a compiler macro
  (lambda (&rest arguments)
    (handler-case (apply function arguments)
      (error () replacement-value))))

(defun ignore-nil (function)
  "Wrap FUNCTION in a closure that returns NIL in case any of the arguments
are NIL."
  (lambda (&rest arguments)
    (when (every #'identity arguments)
      (apply function arguments))))

(defun text-progress-bar (stream n &key
                           (character #\*) (length 80)
                           (deciles? t) (before "~&[") (after "]~%"))
  "Return a closure that displays a progress bar when called with
increments (defaults to 1).  When the second argument is T, index will be set
to the given value (instead of a relative change).

LENGTH determines the number of CHARACTERs to display (not including AFTER and
BEFORE, which are displayed when the closure is first called and after the
index reaches N, respectively).  When DECILES?, characters at every decile
will be replaced by 0,...,9.

When STREAM is NIL, nothing is displayed."
  (unless stream
    (return-from text-progress-bar (lambda ())))
  (let* ((characters (aprog1 (make-string length :initial-element character)
                       (when deciles?
                         (loop for index :below 10 do
                           (replace it (format nil "~d" index)
                                    :start1 (floor (* index length) 10))))))
         (index 0)
         (position 0))
    (lambda (&optional (increment 1) absolute?)
      (when before
        (format stream before)
        (setf before nil))
      (if absolute?
          (progn
            (assert (<= index increment) () "Progress bar can't rewind.")
            (setf index increment))
          (incf index increment))
      (assert (<= index n) () "Index ran above total (~A > ~A)." index n)
      (let ((target-position (floor (* index length) n)))
        (loop while (< position target-position) do
              (princ (aref characters position) stream)
              (incf position)))
      (when (and (= index n) after)
        (format stream after)))))

(declaim (inline within?))
(defun within? (left value right)
  "Return non-nil iff value is in [left,right)."
  (and (<= left value) (< value right)))

(declaim (inline fixnum?))
(defun fixnum? (object)
  "Check of type of OBJECT is fixnum."
  (typep object 'fixnum))

(deftype simple-fixnum-vector ()
  '(simple-array fixnum (*)))

(defun as-simple-fixnum-vector (sequence &optional copy?)
  "Convert SEQUENCE to a SIMPLE-FIXNUM-VECTOR.  When COPY?, make sure that the
they don't share structure."
  (if (and (typep sequence 'simple-fixnum-vector) copy?)
      (copy-seq sequence)
      (coerce sequence 'simple-fixnum-vector)))

(defmacro define-indirect-accessors (specializer slot-accessor
                                     &rest accessors)
  "Define accessor methods for specializer going though a slot."
  (with-unique-names (instance)
    `(progn
       ,@(loop for accessor in accessors collect
               `(defmethod ,accessor ((,instance ,specializer))
                  (,accessor (,slot-accessor ,instance)))))))

(defgeneric keys-and-values (object)
  (:documentation "Return a vector of (cons KEY VALUE) in OBJECT (eg a
hash-table).")
  (:method ((object hash-table))
    (let* ((size (hash-table-count object))
           (result (make-array size))
           (index 0))
      (maphash (lambda (key value)
                 (setf (aref result index) (cons key value))
                 (incf index))
               object)
      result)))

;;;; Thinning
;;;
;;; Thinning is always by a uniform step interval.

(defun thinned-length% (length thinning &optional (start 0))
  "Internal function for calculating the thinned length."
  (ceiling (- length start) thinning))

(defun thin (vector thinning &optional (start 0))
  "Thin vector, keeping every THINNING element, starting at START."
  (let* ((n (length vector))
         (m (thinned-length% n thinning start))
         (result (make-array m :element-type (array-element-type vector))))
    (loop for index below m
          do (setf (aref result index) (aref vector start))
             (incf start thinning))
    result))

(defun thin-to (vector length &optional (rounding :closest))
  "Thin close to the desired length.  ROUNDING can be :CLOSEST, :BELOW,
and :ABOVE, which determines how the length of the result is selected relative
to the desired length."
  (let+ ((vector-length (length vector))
         (above (floor vector-length length))
         (below (ceiling vector-length length)))
   (thin vector (ecase rounding
                  (:below below)
                  (:above above)
                  (:closest
                   (if (< (- length (thinned-length% vector-length below))
                          (- (thinned-length% vector-length above) length))
                       below
                       above))))))

;;;; Aliases for commonly used log bases.

(declaim (inline log10 log2))

(defun log10 (number)
  "Abbreviation for decimal logarithm."
  (log number 10))

(defun log2 (number)
  "Abbreviation for binary logarithm."
  (log number 2))

(declaim (inline 1c))

(defun 1c (number)
  "Return 1-number.  The mnemonic is \"1 complement\", 1- is already a CL
library function."
  (- 1 number))

(defun generate-sequence (result-type size function)
  "Like MAKE-SEQUENCE, but using a function to fill the result."
  (map-into (make-sequence result-type size) function))
