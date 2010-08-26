;;; Copyright Tamas Papp 2010.
;;;
;;; Distributed under the Boost Software License, Version 1.0.  (See
;;; accompanying file LICENSE_1_0.txt or copy at
;;; http://www.boost.org/LICENSE_1_0.txt)
;;;
;;; This copyright notice pertains to all files in this library.

(defsystem #:cl-num-utils
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
    ((:file "misc")
     (:file "seq-and-array")
     (:file "pretty")
     (:file "bins")
     (:file "statistics")
     (:file "interval")
     (:file "histogram")
     (:file "sub")
     (:file "ix")
     (:file "optimization"))))
  :depends-on
  (:cl-utilities :iterate :metabang-bind :anaphora :tpapp-utils))
