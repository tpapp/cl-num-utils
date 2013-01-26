;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite arithmetic-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (arithmetic-tests)
  ivec-test
  (let ((*lift-equality-test* #'equalp))
    (ensure-same (ivec 3) #(0 1 2))
    (ensure-same (ivec -2) #(0 -1))
    (ensure-same (ivec 2 5) #(2 3 4))
    (ensure-same (ivec 0) #())
    (ensure-same (ivec 2 6 2) #(2 4))
    (ensure-same (ivec 6 2 2) #(6 4))
    (ensure-same (ivec -2 -9 3) #(-2 -5 -8))
    (ensure-same (ivec 1 8 2) #(1 3 5 7))))

(addtest (arithmetic-tests)
  (let ((a #(1 2 3))
        (*lift-equality-test* #'==))
   (ensure-same (normalize1 a) #(1/6 1/3 1/2))))
