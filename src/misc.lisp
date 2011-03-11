;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun divides? (number divisor)
  "Test if DIVISOR divides NUMBER without remainder, and if so, return the
  quotient.  Works generally, but makes most sense for rationals."
  (bind (((:values quot rem) (floor number divisor)))
    (when (zerop rem)
      quot)))

(declaim (inline square))
(defun square (number)
  "Return square of NUMBER."
  (expt number 2))

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
    (t (bind (((:values int frac) (floor number)))
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
  "If COPY?, return a copy of array, otherwise the original."
  (if copy?
      (copy-array array)
      array))

(defun convex-combination (a b alpha)
  "Convex combination (1-alpha)*a+alpha*b."
  (+ (* (- 1 alpha) a) (* alpha b)))

(defun vector-last (vector &optional (n 1))
  "Like LAST, but for vectors."
  (aref vector (- (length vector) n)))

(defun common (sequence &key (key #'identity) (test #'eql))
  "If the elements of sequence are the same (converted with KEY, compared with
TEST), return that, otherwise NIL."
  (reduce (lambda (a b)
            (if (funcall test a b)
                a
                (return-from common nil)))
          sequence
          :key key))

(defun common-length (sequences)
  "If sequences have the same length, return that, otherwise NIL."
  (common sequences :key #'length :test #'=))

(defun common-dimensions (arrays)
  "If arrays have the same dimensions, return that, otherwise NIL."
  (common arrays :key #'array-dimensions :test #'equalp))
