(defpackage #:cl-num-utils-tests
  (:use #:cl #:cl-utilities #:iterate
        #:metabang-bind #:anaphora #:cl-num-utils #:lift)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   run-cl-num-utils-tests

   ))
