;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite statistics-tests (tests))

(defun precise-central-moments (sequence)
  "First 4 central moments, calculated using rationals, returned as four values, normalized by the length of the sequence.

Slow, but useful for testing as it does not suffer from approximation error."
  (let+ ((vector (map 'vector #'rational sequence))
         (n (length vector))
         (mean (/ (reduce #'+ vector) n))
         ((&flet central-m (degree)
            (/ (reduce #'+ vector
                       :key (lambda (v)
                              (expt (- v mean) degree)))
               n))))
    (values mean (central-m 2) (central-m 3) (central-m 4))))

(defun random-floats (n mean &optional (element-type 'double-float))
  "Return a N-element vector of random floats (with given ELEMENT-TYPE).  A uniform random number from either [-1,0] or [0,3] (with equal probability) is added to MEAN, which ensures nonzero third and fourth central moments.  Higher abolute value of MEAN makes the calculation of higher central moments more ill-conditioned when using floats."
  (let ((mean (coerce mean element-type))
        (one (coerce 1 element-type)))
    (ao:generate* element-type
                  (lambda ()
                    (let ((v (random one)))
                      (if (zerop (random 2))
                          (- mean v)
                          (+ mean (* 3 v)))))
                  n)))

(defun test-moments (n mean &optional (element-type 'double-float))
  (let+ ((v (random-floats n mean element-type))
         ((&values m-p m2-p m3-p m4-p) (precise-central-moments v))
         ((&accessors-r/o mean central-m2 central-m3 central-m4)
          (central-sample-moments v 4))
         (*num=-tolerance* 1e-8))
    (assert-equality #'num= mean m-p)
    (assert-equality #'num= central-m2 m2-p)
    (assert-equality #'num= central-m3 m3-p)
    (assert-equality #'num= central-m4 m4-p)))

(deftest central-moments-test1 (statistics-tests)
  (test-moments 1000 0)
  (test-moments 1000 10)
  (test-moments 1000 100)
  (test-moments 1000 1000000))

