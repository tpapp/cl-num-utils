(defpackage #:cl-num-utils-tests
  (:use cl alexandria iterate metabang-bind anaphora cl-num-utils lift)
  (:shadowing-import-from cl-num-utils mean)
  (:export

   run-cl-num-utils-tests

   ))
