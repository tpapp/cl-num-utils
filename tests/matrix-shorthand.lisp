;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite matrix-shorthand-suite (tests))

(deftest lotr-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (lotr-mx (lotr-mx t
                   (1)
                   (3 4))))
    (assert-equality #'num= (lower-triangular-matrix matrix) lotr-mx)
    (assert-equality #'num= lotr-mx (lotr-mx t
                                      (1 9) ; 9 should be ignored
                                      (3 4)))
    (assert-equality #'num= (lotr-mx t
                              (1 2 3)
                              (3 4 5))
        (lotr-mx t
          (1 2 17)
          (3 4 5)))
    (assert-equality #'num= (lotr-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (lotr-mx t
          (1 19)
          (3 4)
          (5 6)))))

(deftest uptr-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (uptr-mx (uptr-mx t
                   (1 2)
                   (3 4))))
    (assert-equality #'num= (upper-triangular-matrix matrix) uptr-mx)
    (assert-equality #'num= uptr-mx (uptr-mx t
                                      (1 2)
                                      (9 4))) ; 9 should be ignored
    (assert-equality #'num= (uptr-mx t
                              (1 2 3)
                              (3 4 5))
        (uptr-mx t
          (1 2 3)
          (19 4 5)))
    (assert-equality #'num= (uptr-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (uptr-mx t
          (1 2)
          (3 4)
          (19 6)))))

(deftest herm-shorthand-test (matrix-shorthand-suite)
  (let ((matrix #2A((1 2)
                    (3 4)))
        (herm-mx (herm-mx t
                   (1)
                   (3 4))))
    (assert-equality #'num= herm-mx (hermitian-matrix matrix))
    (assert-equality #'num= herm-mx (herm-mx t
                                      (1 9) ; 9 should be ignored
                                      (3 4)))
    (assert-condition error (herm-mx t
                              (1 2 3)
                              (3 4 5)))))

(deftest diag-shorthand-test (matrix-shorthand-suite)
  (assert-equality #'num= (diag-mx t 1 2 3) (diagonal-matrix #(1 2 3))))

(deftest vec-shorthand-test (matrix-shorthand-suite)
  (assert-equality #'num= (vec t 1 2 3) #(1 2 3)))
