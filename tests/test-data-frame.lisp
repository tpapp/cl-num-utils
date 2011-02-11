;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite data-frame-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (data-frame-tests)
  simple-data-frame-tests
  (let* ((matrix (ia 2 3))
         (sub-matrix #2A((0 2) (3 5)))
         (sub-vector #(1 4))
         (keys '(a b c))
         (df (make-data-frame matrix keys)))
    (ensure-same (sub df t 'b) sub-vector)
    (ensure-same (sub df t #(a c)) sub-matrix)
    ;; should pass through regular arguments
    (ensure-same (sub df t t) matrix)
    (ensure-same (sub df t 1) sub-vector)
    (ensure-same (sub df t #(0 2)) sub-matrix)
    (ensure-same (sub df t (si 0 nil)) matrix)))

