;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite matrix-suite (tests))

(deftest wrapped-univariate-operation (matrix-suite)
  (assert-equality #'num= (e- (uptr-mx t 2)) (uptr-mx t -2))
  (assert-equality #'num= (e/ (uptr-mx t 2)) (uptr-mx t 0.5))
  (assert-equality #'num= (e+ (uptr-mx t 2)) (uptr-mx t 2)))

(defun do-matrix-convert-ops (test &key converts
                                        (ops (list #'e+ #'e- #'e*)))
  "Funcall TEST with CONVERT and each operation in OPs."
  (mapc (lambda (convert)
          (mapc (curry #'funcall test convert) ops))
        converts))

(defun assert-distributive-convert-op (a b convert op)
  "Check that OP distributes over CONVERT."
  (assert-equality #'num= (funcall convert (funcall op a b))
      (funcall op (funcall convert a) (funcall convert b))))

(deftest wrapped-bivariate-operation (matrix-suite)
  (do-matrix-convert-ops (curry #'assert-distributive-convert-op
                                (mx t
                                  (1 2)
                                  (3 4))
                                (mx t
                                  (5 7)
                                  (11 13)))
    (list #'hermitian-matrix
          #'lower-triangular-matrix
          #'upper-triangular-matrix)))

(deftest wrapped-bivariate-to-array (matrix-suite)
  (let+ ((a (mx t
              (1 2)
              (3 4)))
         (b (mx t
              (5 7)
              (11 13))))
    (do-matrix-convert-ops (lambda (convert op)
                             (assert-equality #'num= (funcall op a b)
                                 (funcall op (funcall convert a) b))
                             (assert-equality #'num= (funcall op a b)
                                 (funcall op a (funcall convert b))))
      (list #'hermitian-matrix
            #'lower-triangular-matrix
            #'upper-triangular-matrix))))

(deftest diagonal-test (matrix-suite)
  (do-matrix-convert-ops (curry #'assert-distributive-convert-op
                                (vec t 1 2 3 4)
                                (vec t 5 7 11 13))
    (list #'diagonal-matrix)))
