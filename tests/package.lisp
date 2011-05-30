(defpackage #:cl-num-utils-tests
  (:use cl alexandria iterate let-plus anaphora cl-num-utils lift)
  (:shadowing-import-from cl-num-utils mean variance)
  (:export

   run-cl-num-utils-tests

   ))
