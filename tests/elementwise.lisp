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
    (ensure-same (e+ a) (e+ a 0))
    (ensure-same (e* a) (e* a 1))
    (ensure-same (e- a) (e- 0 a))
    (ensure-same (e/ a) (e/ 1 a))))

(addtest (elementwise-tests)
  stack-tests
  (let ((a (array* '(2 3) t
                   1 2 3
                   4 5 6))
        (b (array* '(2 2) t
                   3 5
                   7 9)))
    (ensure-same (stack 'double-float :h a b)
                 (array* '(2 5) 'double-float
                         1 2 3 3 5
                         4 5 6 7 9))
    (ensure-same (stack t :v (transpose a) b)
                 #2A((1 4)
                     (2 5)
                     (3 6)
                     (3 5)
                     (7 9)))
    (ensure-same (stack 'fixnum :v a #(7 8 9) 10)
                 (array* '(4 3) 'fixnum
                          1 2 3
                          4 5 6
                          7 8 9
                          10 10 10))
    (ensure-same (stack t :h b #(1 2) b 9 b)
                 (array* '(2 8) t
                         3 5 1 3 5 9 3 5
                         7 9 2 7 9 9 7 9))
    (ensure-same (stack nil :h
                        (vector* 'double-float 1 2)
                        (vector* 'double-float 3 4))
                 (array* '(2 2) 'double-float
                         1 3
                         2 4))))
