;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite sub-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (sub-tests)
  test-sub
  (let ((a (ia 3 4)))
    (ensure-same (sub a (&r 0 -1) (&r 0 -1))
                 #2A((0 1 2)
                     (4 5 6)))
    (ensure-same (sub a (&r 1 -1) t)
                 #2A((4 5 6 7)))
    (ensure-same (sub a 1 t)
                 #(4 5 6 7))
    (ensure (not (equalp (sub a 1 t)
                         #2A((4 5 6 7)))))))

(addtest (sub-tests)
  test-setf-sub
  (let ((b (ia 2 3)))
    (let ((a (ia 3 4)))
      (ensure-same (setf (sub a (&r 1 0) (&r 1 0)) b) b)
      (ensure-same a #2A((0 1 2 3)
                         (4 0 1 2)
                         (8 3 4 5)))
      (ensure-same b (ia 2 3)))
    (let ((a (ia 3 4)))
      (ensure-same (setf (sub a (&r 0 -1) #(3 2 1)) b) b)
      (ensure-same a #2A((0 2 1 0)
                         (4 5 4 3)
                         (8 9 10 11)))
      (ensure-same b (ia 2 3)))))

(addtest (sub-tests)
  map-columns
  (ensure-same (map-columns #'sum (ia 3 4))
               #(12 15 18 21))
  (ensure-same (map-columns (lambda (col) (vector (sum col) (mean col))) (ia 3 4))
               #2A((12 15 18 21)
                   (4 5 6 7))))

(addtest (sub-tests)
  map-rows
  (ensure-same (map-rows #'sum (ia 4 3))
               #(3 12 21 30))
  (ensure-same (map-rows (lambda (col) (vector (sum col) (mean col))) (ia 4 3))
               #2A((3 1)
                   (12 4)
                   (21 7)
                   (30 10))))
