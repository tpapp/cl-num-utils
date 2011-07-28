;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite misc-tests (cl-num-utils-tests)
  ())

(addtest (misc-tests)
  bracket-test
  (let ((a #(0 1 2 3 4 3 2 1 0)))
    (ensure-same (bracket #'plusp a) (cons 1 8))
    (ensure-same (bracket (curry #'<= 3) a) (cons 3 6))
    (ensure-same (bracket (curry #'<= 5) a) nil)
    (ensure-same (bracket t #(nil nil nil t t nil nil)) (cons 3 5))))

