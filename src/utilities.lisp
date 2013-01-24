;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:cl-num-utils.utilities
  (:use #:cl #:alexandria #:let-plus)
  (:export
   #:gethash*
   #:splice-when
   #:splice-awhen
   #:check-types
   #:define-with-multiple-bindings))

(cl:in-package #:cl-num-utils.utilities)

(defmacro gethash* (key hash-table
                    &optional (datum "Key not found.")
                    &rest arguments)
  "Like GETHASH, but checking that KEY is present and raising the given error if not."
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

(defmacro check-types ((&rest arguments) type)
  "CHECK-TYPE for multiple places of the same type.  Each argument is either a place, or a list of a place and a type-string."
  `(progn
     ,@(loop
         for argument :in arguments
         collecting (if (atom argument)
                        `(check-type ,argument ,type)
                        (let+ (((place type-string) argument))
                          `(check-type ,place ,type ,type-string))))))

(defmacro define-with-multiple-bindings
    (macro &key
           (plural (intern (format nil "~aS" macro)))
           (docstring (format nil "Multiple binding version of ~(~a~)." macro)))
  "Define a version of MACRO with multiple arguments, given as a list.  Application of MACRO will be nested.  The new name is the plural of the old one (generated using format by default)."
  `(defmacro ,plural (bindings &body body)
     ,docstring
     (if bindings
         `(,',macro ,(car bindings)
                    (,',plural ,(cdr bindings)
			       ,@body))
         `(progn ,@body))))
