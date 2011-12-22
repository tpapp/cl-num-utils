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
         (df (matrix-to-data-frame matrix #(a b c)))
         ;; (sub-df (make-data-frame sub-matrix (sub keys #(0 2))))
         (*lift-equality-test* #'==))
    (ensure-same (sub df t 'b) sub-vector)
    (ensure-same (sub df t (vector 'a 'c))
                 (matrix-to-data-frame sub-matrix #(a c)))
    ;; should pass through regular arguments
    (ensure-same (sub df t t) df)))

(addtest (data-frame-tests)
  data-frame-setf-tests
  (let* ((matrix (ia 3 4))
         (df (matrix-to-data-frame matrix '(a b c d)))
         (sub-vector (ia 3)))
    (setf (sub df t 'c) sub-vector
          (sub matrix t 2) sub-vector)
    (ensure-same (as-array df) matrix)))

;; (addtest (data-frame-tests)
;;   data-frame-filter-tests
;;   (let* ((matrix (ia 4 3))
;;          (keys '(a b (c foo)))
;;          (*lift-equality-test* #'equalp)
;;          (expected-result (make-data-frame #2A((6 7 8)) keys))
;;          (df (make-data-frame matrix keys)))
;;     (ensure-same (with-filter-data-frame df (a (c '(c foo)))
;;                    (and (evenp a) (= c 8)))
;;                  expected-result)))

;; (addtest (data-frame-tests)
;;   (let* ((matrix (array* '(2 3) t
;;                          1 2 3
;;                          5 6 7))
;;          (shrunk-matrix (array* '(2 1) t 2 6))
;;          (data-frame (make-data-frame matrix '(a b c)))
;;          (shrunk-data-frame (shrink-rows data-frame :predicate #'evenp)))
;;     (ensure-same (as-array shrunk-data-frame) shrunk-matrix)
;;     (ensure-same (ix-keys shrunk-data-frame) (sub (ix-keys data-frame) (si 1 2)))))
