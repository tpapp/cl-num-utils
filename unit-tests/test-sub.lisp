;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defun ia (&rest dimensions)
  "Return an array with given dimensions, filled with integers from 0,
in row-major order.  For testing purposes."
  (aprog1 (make-array dimensions)
    (iter
      (for i :from 0 :below (array-total-size it))
      (setf (row-major-aref it i) i))))

(deftestsuite sub-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (sub-tests)
  test-sub
  (let ((a (ia 3 4)))
    (ensure-same (sub a '(0 . -1) '(0 . -1))
                 #2A((0 1 2)
                     (4 5 6)))
    (ensure-same (sub a '(1 . -1) t)
                 #2A((4 5 6 7)))
    (ensure-same (sub a 1 t)
                 #(4 5 6 7))
    (ensure (not (equalp (sub a 1 t)
                         #2A((4 5 6 7)))))))

(addtest (sub-tests)
  test-setf-sub
  (let ((b (ia 2 3)))
    (let ((a (ia 3 4)))
      (ensure-same (setf (sub a '(1 . 0) '(1 . 0)) b) b)
      (ensure-same a #2A((0 1 2 3)
                         (4 0 1 2)
                         (8 3 4 5)))
      (ensure-same b (ia 2 3)))
    (let ((a (ia 3 4)))
      (ensure-same (setf (sub a '(0 . -1) #(3 2 1)) b) b)
      (ensure-same a #2A((0 2 1 0)
                         (4 5 4 3)
                         (8 9 10 11)))
      (ensure-same b (ia 2 3)))))

(addtest (sub-tests)
  map-columns
  (ensure-same (map-columns (ia 3 4) #'sum)
               #(12 15 18 21))
  (ensure-same (map-columns (ia 3 4) (lambda (col) (vector (sum col) (mean col))))
               #2A((12 15 18 21)
                   (4 5 6 7))))

(addtest (sub-tests)
  map-rows
  (ensure-same (map-rows (ia 4 3) #'sum)
               #(3 12 21 30))
  (ensure-same (map-rows (ia 4 3) (lambda (col) (vector (sum col) (mean col))))
               #2A((3 1)
                   (12 4)
                   (21 7)
                   (30 10))))
