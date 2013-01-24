;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:cl-num-utils.utilities
  (:use #:cl #:alexandria)
  (:export
   #:gethash*
   #:splice-when
   #:splice-awhen))

(cl:in-package #:cl-num-utils.utilities)

(defmacro gethash* (key hash-table
                    &optional (datum "Key not found.")
                    &rest arguments)
  "Like GETHASH, but checking that KEY is present and raising the given
error if not."
  (with-unique-names (value present?)
    `(multiple-value-bind (,value ,present?) (gethash ,key ,hash-table)
       (assert ,present? () ,datum ,@arguments)
       ,value)))

(defmacro splice-when (test &body forms)
  "Similar to when, but wraps the result in list.

Example: `(,foo ,@(splice-when add-bar? bar))"
  `(when ,test
     (list
      (progn ,@forms))))

(defmacro splice-awhen (test &body forms)
  "Similar to splice-when, but binds IT to test."
  `(awhen ,test
     (list
      (progn ,@forms))))
