;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite matrix-shorthand-suite (tests))

(deftest lower-triangular-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (lower-triangular-mx (lower-triangular-mx t
                               (1)
                               (3 4))))
    (assert-equality #'num= (lower-triangular-matrix matrix) lower-triangular-mx)
    (assert-equality #'num= lower-triangular-mx (lower-triangular-mx t
                                                  (1 9) ; 9 should be ignored
                                                  (3 4)))
    (assert-equality #'num= (lower-triangular-mx t
                              (1 2 3)
                              (3 4 5))
        (lower-triangular-mx t
          (1 2 17)
          (3 4 5)))
    (assert-equality #'num= (lower-triangular-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (lower-triangular-mx t
          (1 19)
          (3 4)
          (5 6)))))

(deftest upper-triangular-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (upper-triangular-mx (upper-triangular-mx t
                   (1 2)
                   (3 4))))
    (assert-equality #'num= (upper-triangular-matrix matrix) upper-triangular-mx)
    (assert-equality #'num= upper-triangular-mx (upper-triangular-mx t
                                      (1 2)
                                      (9 4))) ; 9 should be ignored
    (assert-equality #'num= (upper-triangular-mx t
                              (1 2 3)
                              (3 4 5))
        (upper-triangular-mx t
          (1 2 3)
          (19 4 5)))
    (assert-equality #'num= (upper-triangular-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (upper-triangular-mx t
          (1 2)
          (3 4)
          (19 6)))))

(deftest hermitian-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (hermitian-mx (hermitian-mx t
                        (1)
                        (3 4))))
    (assert-equality #'num= hermitian-mx (hermitian-matrix matrix))
    (assert-equality #'num= hermitian-mx (hermitian-mx t
                                           (1 9) ; 9 should be ignored
                                           (3 4)))
    (assert-condition error (hermitian-mx t
                              (1 2 3)
                              (3 4 5)))))

(deftest diagonal-shorthand-test (matrix-shorthand-suite)
  (assert-equality #'num= (diagonal-mx t 1 2 3) (diagonal-matrix #(1 2 3))))

(deftest vec-shorthand-test (matrix-shorthand-suite)
  (assert-equality #'num= (vec t 1 2 3) #(1 2 3)))
