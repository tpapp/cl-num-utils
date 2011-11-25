;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite quadrature-tests (cl-num-utils-tests)
  ()
  (:equality-test (==* 1d-10)))

(defmacro test-romberg (list &rest rest)
  (with-unique-names (function a b value)
    `(let+ (((,function ,a ,b ,value) ,list))
       (ensure-same (romberg-quadrature ,function ,a ,b 
                                        ,@rest)
                    ,value))))

(addtest (quadrature-tests)
  integration1
  (let ((q1 (list (constantly 1d0) 0 2 2d0))
        (q2 (list #'identity 1 5 12d0))
        (q3 (list (lambda (x) (/ (exp (- (/ (expt x 2) 2)))
                                 (sqrt (* 2 pi))))
                  0 1 0.3413447460685429d0)))
    (test-romberg q1)
    (test-romberg q1 :open? t)
    (test-romberg q2)
    (test-romberg q2 :open? t)
    (test-romberg q3 :epsilon 1d-9) 
    (test-romberg q3 :epsilon 1d-9 :open? t)))
