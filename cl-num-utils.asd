;;; Copyright Tamas Papp 2010.
;;;
;;; Distributed under the Boost Software License, Version 1.0.  (See
;;; accompanying file LICENSE_1_0.txt or copy at
;;; http://www.boost.org/LICENSE_1_0.txt)
;;;
;;; This copyright notice pertains to all files in this library.

(asdf:defsystem #:cl-num-utils
  :description "Numerical utilities for Common Lisp"
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :serial t
  :components 
  ((:module 
    "package-init"
    :pathname #P"src/"
    :components
    ((:file "package")))
   (:module
    "utilities"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "macros")
     (:file "misc")
     (:file "arithmetic")
     (:file "array")
     (:file "pretty")
     (:file "bins")
     (:file "statistics")
     (:file "interval")
     ;; (:file "histogram")
     (:file "sub")
     (:file "elementwise")
     (:file "ix")
     (:file "data-frame")
     ;; (:file "interaction")
     (:file "optimization"))))
  :depends-on (alexandria iterate let-plus anaphora))
