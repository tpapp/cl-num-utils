;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(define-condition reached-maximum-iterations ()
  ((n :initarg :n :documentation "Number of iterations.")))
