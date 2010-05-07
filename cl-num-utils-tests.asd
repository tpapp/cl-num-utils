(defsystem :cl-num-utils-tests
  :description "Unit tests for CL-NUM-UTILS.."
  :version "alpha"
  :author "Tamas K Papp <tkpapp@gmail.com"
  :license "Same as CL-NUM-UTILS--this is part of the CL-NUM-UTILS library."
  :serial t
  :components
  ((:module 
    "package-init"
    :pathname #P"unit-tests/"
    :components
    ((:file "package")))
   (:module
    "setup"
    :pathname #P"unit-tests/"
    :components
    ((:file "setup")))
   (:module 
    "tests"
    :pathname #P"unit-tests/"
    :components
    ((:file "test-seq")
     (:file "test-sub"))))
  :depends-on
  (:cl-utilities :iterate :metabang-bind :anaphora :lift
                 :cl-num-utils))
