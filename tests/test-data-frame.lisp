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
         (df (make-data-frame matrix keys))
         (sub-df (make-data-frame sub-matrix (sub keys #(0 2))))
         (*lift-equality-test* #'equalp))
    (ensure-same (sub df t 'b) sub-vector)
    (ensure-same (sub df t #(a c)) sub-df)
    ;; should pass through regular arguments
    (ensure-same (sub df t t) (make-data-frame matrix keys))
    (ensure-same (sub df t 1) sub-vector)
    (ensure-same (sub df t #(0 2)) sub-df)
    (ensure-same (sub df t (si 0 nil)) (make-data-frame matrix keys))))

(addtest (data-frame-tests)
  data-frame-setf-tests
  (let* ((matrix (ia 3 4))
         (keys '(a b c d))
         (df (make-data-frame (copy-array matrix) keys :copy? t))
         (sub-vector (ia 3)))
    (setf (sub df t 'c) sub-vector
          (sub matrix t 2) sub-vector)
    (ensure-same (data-frame-matrix df) matrix)))

(addtest (data-frame-tests)
  data-frame-filter-tests
  (let* ((matrix (ia 4 3))
         (keys '(a b (c foo)))
         (*lift-equality-test* #'equalp)
         (expected-result (make-data-frame #2A((6 7 8)) keys))
         (df (make-data-frame matrix keys)))
    (ensure-same (with-filter-data-frame df (a (c '(c foo)))
                   (and (evenp a) (= c 8)))
                 expected-result)))
