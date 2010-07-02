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

(defun xor (a b)
  "Exclusive or."
  (if a (not b) b))

(defun bic (a b)
  "Biconditional."
  (if a b (not b)))

(define-modify-macro multf (&rest values) * "Multiply by the arguments")
