;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite elementwise-tests (cl-num-utils-tests)
  ()
  (:equality-test #'array=))

(addtest (elementwise-tests)
  emap-type-tests
  (let ((*lift-equality-test* (lambda (type1 type2)
                                (type= type1 (upgraded-array-element-type type2)))))
    (ensure-same (emap-common-numeric-type 'single-float 'double-float)
                 'double-float)
    (ensure-same (emap-common-numeric-type '(complex single-float) 'double-float)
                 '(complex double-float))
    (ensure-same (emap-common-numeric-type 'fixnum 'double-float) 'double-float)
    (ensure-same (emap-common-numeric-type 'fixnum 'integer) 'integer)))

(addtest (elementwise-tests)
  e-operations-tests
  (let ((*lift-equality-test* #'array=)
        (a (array* '(2 3) 'double-float
                   1 2 3
                   4 5 6))
        (b (array* '(2 3) 'single-float
                   2 3 5
                   7 11 13)))
    (ensure-same (e+ a b) (array* '(2 3) 'double-float
                                  3 5 8
                                  11 16 19))
    (ensure-same (e* a 2) (array* '(2 3) 'double-float
                                  2 4 6
                                  8 10 12))
    (ensure-same (e+ a 2 b) (e+ (e+ a b) 2))
    (ensure-error (e/ a 0))             ; division by 0
    (ensure-error (e+ a (array* '(1 1) 2))) ; dimension incompatibility
    ))


