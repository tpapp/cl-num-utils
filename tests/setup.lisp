(in-package :cl-num-utils-tests)

;; TEST SUITES

(deftestsuite cl-num-utils-tests () ())

;; EXTERNAL

(defun run ()
  "Run all the tests for CL-NUM-UTILS."
  (run-tests :suite 'cl-num-utils-tests))
