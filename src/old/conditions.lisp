;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(define-condition reached-maximum-iterations ()
  ((n :initarg :n :documentation "Number of iterations.")))

(define-condition internal-error ()
  ()
  (:report "Internal error.  Please report it as a bug.")
  (:documentation "An error that is not supposed to happen if the code is
  correct.  May be the result of numerical imprecision.  Please report it as a
  bug."))

(define-condition not-implemented ()
  ()
  (:report "This functionality is not implemented yet.  If you need it, please
  report it as an issue.")
  (:documentation "Placeholder condition for functionality that is not
  implemented yet."))
