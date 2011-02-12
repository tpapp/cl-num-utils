;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite interactions-tests (cl-num-utils-tests)
  ())

(addtest (interactions-tests)
  simple-interactions-tests
  (let ((*lift-equality-test* #'equalp))
    (bind (((:slots indexes keys)
            (interaction #(0 0 1) #(0 1 1))))
      (ensure-same indexes #(0 1 2))
      (ensure-same keys #(#(0 0) #(0 1) #(1 1))))
    (bind (((:slots indexes keys)
            (interaction #(0 2 1) #(0 1 2))))
      (ensure-same indexes #(0 2 1))
      (ensure-same keys #(#(0 0) #(1 2) #(2 1))))))
