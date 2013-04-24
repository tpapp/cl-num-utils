;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:cl-num-utils)
    (defpackage #:cl-num-utils
      (:nicknames #:clnu)
      (:use #:cl))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:cl-num-utils)

  (flet ((reexport (package)
           "Reexport all external symbols of package."
           (let ((package (find-package package)))
             (do-external-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (import symbol)
                 (export symbol))))))
    (reexport '#:cl-num-utils.arithmetic)
    (reexport '#:cl-num-utils.chebyshev)
    (reexport '#:cl-num-utils.elementwise)
    (reexport '#:cl-num-utils.interval)
    (reexport '#:cl-num-utils.matrix)
    (reexport '#:cl-num-utils.num=)
    (reexport '#:cl-num-utils.statistics)
    (reexport '#:cl-num-utils.utilities)
    (reexport '#:cl-num-utils.rootfinding)))
