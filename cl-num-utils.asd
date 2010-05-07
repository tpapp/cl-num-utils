(defsystem #:cl-num-utils
  :description "Numerical utilities for Common Lisp"
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "BSD without advertising clause"
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
    ((:file "seq")
     (:file "pretty")
     (:file "bins")
     (:file "statistics")
     (:file "interval")
     (:file "histogram")
     (:file "sub"))))
  :depends-on
  (:cl-utilities :iterate :metabang-bind :anaphora))
