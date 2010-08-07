(defsystem :cl-num-utils-tests
  :description "Unit tests for CL-NUM-UTILS.."
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com"
  :license "Same as CL-NUM-UTILS--this is part of the CL-NUM-UTILS library."
  :serial t
  :components
  ((:module 
    "package-init"
    :pathname #P"tests/"
    :components
    ((:file "package")))
   (:module
    "setup"
    :pathname #P"tests/"
    :components
    ((:file "setup")))
   (:module 
    "tests"
    :pathname #P"tests/"
    :components
    ((:file "test-seq")
     (:file "test-bins")
     (:file "test-sub")
     (:file "test-statistics"))))
  :depends-on
  (:cl-utilities :iterate :metabang-bind :anaphora :lift
                 :cl-num-utils))
