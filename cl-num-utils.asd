;;; Copyright Tamas Papp 2010.
;;;
;;; Distributed under the Boost Software License, Version 1.0.  (See
;;; accompanying file LICENSE_1_0.txt or copy at
;;; http://www.boost.org/LICENSE_1_0.txt)
;;;
;;; This copyright notice pertains to all files in this library.

(asdf:defsystem #:cl-num-utils
  :description "Numerical utilities for Common Lisp"
  :version "0.1"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:anaphora
               #:alexandria
               #:array-operations
               #:cl-slice
               #:let-plus)
  :pathname "src/"
  :serial t
  :components
  ((:file "utilities")
   (:file "num=")
   (:file "arithmetic")
   (:file "elementwise")
   (:file "extended-real")
   (:file "interval")
   (:file "print-matrix")
   (:file "matrix")
   (:file "matrix-shorthand")
   (:file "statistics")
   (:file "chebyshev")
   (:file "rootfinding")
   (:file "common-package")))

(asdf:defsystem #:cl-num-utils-tests
  :description "Unit tests for CL-NUM-UTILS.."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Same as CL-NUM-UTILS -- this is part of the CL-NUM-UTILS library."
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-num-utils
               #:clunit)
  :pathname "tests/"
  :serial t
  :components
  ((:file "setup")
   ;; in alphabetical order
   (:file "arithmetic")
   (:file "chebyshev")
   (:file "elementwise")
   (:file "extended-real")
   (:file "interval")
   (:file "matrix")
   (:file "matrix-shorthand")
   (:file "num=")
   (:file "statistics")
   (:file "utilities")
   (:file "rootfinding")))
