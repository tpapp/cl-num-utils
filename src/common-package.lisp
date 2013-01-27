;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-num-utils
  (:nicknames #:clnu)
  (:use #:cl))

(in-package #:cl-num-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((reexport (package)
           "Reexport all external symbols of package."
           (let ((package (find-package package)))
             (do-external-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (import symbol)
                 (export symbol))))))
    (reexport '#:cl-num-utils.utilities)
    (reexport '#:cl-num-utils.arithmetic)
    (reexport '#:cl-num-utils.interval)
    (reexport '#:cl-num-utils.interval)
    (reexport '#:cl-num-utils.statistics)
    (reexport '#:cl-num-utils.num=)
    (reexport '#:cl-num-utils.chebyshev)))
