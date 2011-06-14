(defsystem :cl-num-utils-tests
  :description "Unit tests for CL-NUM-UTILS.."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Same as CL-NUM-UTILS -- this is part of the CL-NUM-UTILS library."
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
    ((:file "setup")
     (:file "utilities")))
   (:module 
    "tests"
    :pathname #P"tests/"
    :components
    ((:file "arithmetic")
     (:file "array")
     (:file "bins")
     (:file "sub")
     (:file "elementwise")
     (:file "statistics")
     (:file "layout")
     (:file "data-frame")
     ;; (:file "interactions")
     )))
  :depends-on
  (iterate metabang-bind anaphora lift alexandria cl-num-utils))
