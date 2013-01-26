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
               #:let-plus)
  :pathname #P"src/"
  :serial t
  :components
  ((:file "utilities")
   (:file "statistics")))

(asdf:defsystem :cl-num-utils-tests
  :description "Unit tests for CL-NUM-UTILS.."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Same as CL-NUM-UTILS -- this is part of the CL-NUM-UTILS library."
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-num-utils
               #:clunit)
  :pathname #P"tests/"
  :serial t
  :components
  ((:file "setup")))
