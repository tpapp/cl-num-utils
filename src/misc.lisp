;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun divides? (number divisor)
  "Test if DIVISOR divides NUMBER without remainder, and if so, return the
  quotient.  Works generally, but makes most sense for rationals."
  (bind (((:values quot rem) (floor number divisor)))
    (when (zerop rem)
      quot)))
