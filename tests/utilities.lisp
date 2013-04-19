;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite utilities-tests (tests))

(deftest gethash-test (utilities-tests)
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'a table) 1)
    (assert-eql 1 (gethash* 'a table))
    (assert-condition error (gethash* 'b table))))

(deftest biconditional-test (utilities-tests)
  (assert-true (bic t t))
  (assert-true (bic nil nil))
  (assert-false (bic t nil))
  (assert-false (bic nil t)))

(deftest splice-when (utilities-tests)
  (assert-equal '(a b c) `(a ,@(splice-when t 'b) c))
  (assert-equal '(a c) `(a ,@(splice-when nil 'b) c))
  (assert-equal '(a b c) `(a ,@(splice-awhen 'b it) c))
  (assert-equal '(a c) `(a ,@(splice-awhen (not 'b) it) c)))

;;; FIXME: write tests for other utilities
