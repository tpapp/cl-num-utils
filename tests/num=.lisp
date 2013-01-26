;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite num=-tests (tests))

(deftest num=-number-test (num=-tests)
  (let ((*num=-tolerance* 1e-3))
    (assert-equality #'num= 1 1)
    (assert-equality #'num= 1 1.0)
    (assert-equality #'num= 1 1.001)
    (assert-false (num= 1 2))
    (assert-false (num= 1 1.01))))

(deftest num=-list-test (num=-tests)
  (let ((*num=-tolerance* 1e-3))
    (assert-equality #'num= nil nil)
    (assert-equality #'num= '(1) '(1.001))
    (assert-equality #'num= '(1 2) '(1.001 1.999))
    (assert-false (num= '(0 1) '(0 1.02)))
    (assert-false (num= nil '(1)))))

(deftest num=-array-test (num=-tests)
  (let* ((*num=-tolerance* 1e-3)
         (a #(0 1 2))
         (b #2A((0 1)
                (2 3))))
    (assert-equality #'num= a a)
    (assert-equality #'num= a #(0 1.001 2))
    (assert-equality #'num= a #(0 1.001 2.001))
    (assert-equality #'num= b b)
    (assert-equality #'num= b #2A((0 1)
                                (2.001 3)))
    (assert-false (num= a b))
    (assert-false (num= a #(0 1)))
    (assert-false (num= a #(0 1.01 2)))
    (assert-false (num= b #2A((0 1))))
    (assert-false (num= b #2A((0 1.01)
                            (2 3))))))

(defstruct num=-test-struct
  "Structure for testing DEFINE-STRUCTURE-num=."
  a b)

(define-structure-num= num=-test-struct a b)

(deftest num=-structure-test (num=-tests)
  (let ((*num=-tolerance* 1e-3)
        (a (make-num=-test-struct :a 0 :b 1))
        (b (make-num=-test-struct :a "string" :b nil)))
    (assert-equality #'num= a a)
    (assert-equality #'num= a (make-num=-test-struct :a 0 :b 1))
    (assert-equality #'num= a (make-num=-test-struct :a 0 :b 1.001))
    (assert-false (num= a (make-num=-test-struct :a 0 :b 1.01)))
    (assert-equality #'num= b b)
    (assert-false (num= a b))))
