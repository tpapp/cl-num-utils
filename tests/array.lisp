;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite array-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (array-tests)
  map-columns
  (ensure-same (map-columns #'sum (ia 3 4))
               #(12 15 18 21))
  (ensure-same (map-columns (lambda (col) (vector (sum col) (mean col))) (ia 3 4))
               #2A((12 15 18 21)
                   (4 5 6 7))))

(addtest (array-tests)
  map-rows
  (ensure-same (map-rows #'sum (ia 4 3))
               #(3 12 21 30))
  (ensure-same (map-rows (lambda (col) (vector (sum col) (mean col))) (ia 4 3))
               #2A((3 1)
                   (12 4)
                   (21 7)
                   (30 10))))

(addtest (array-tests)
  reshape-calculate-dimensions
  (ensure-same (reshape-calculate-dimensions #(1 2 3) 6) #(1 2 3))
  (ensure-same (reshape-calculate-dimensions #(1 t 3) 6) #(1 2 3))
  (ensure-same (reshape-calculate-dimensions #(1 t 3) 0) #(1 0 3))
  (ensure-same (reshape-calculate-dimensions 6 6) #(6))
  (ensure-same (reshape-calculate-dimensions t 6) #(6))
  (ensure-error (reshape-calculate-dimensions #(1 t t 3) 6))
  (ensure-error (reshape-calculate-dimensions #(1 t 0 3) 6)))

(addtest (array-tests)
  reshape
  (let ((a (ia 3 4))
        (a-reshaped-rm #2A((0 1 2)
                           (3 4 5)
                           (6 7 8)
                           (9 10 11))))
    (ensure-same (reshape a '(4 t) :row-major) a-reshaped-rm)
    (ensure-same (reshape a '(4 t) :row-major :copy t) a-reshaped-rm)
    (ensure-same (reshape a '(4 t) :column-major) 
                 #2A((0 5 10)
                     (4 9 3)
                     (8 2 7)
                     (1 6 11)))))

(addtest (array-tests)
  rows-and-columns
  (let ((a #2A((1 2)
               (3 4)
               (5 6)))
        (rows (vector #(1 2) #(3 4) #(5 6)))
        (columns (vector #(1 3 5) #(2 4 6))))
    (ensure-same (rows a) rows)
    (ensure-same (columns a) columns)))

(addtest (array-tests)
  pref
  (let ((matrix #2A((0 1)
                    (2 3)
                    (4 5)))
        (vector #(0 1 2 3)))
    (ensure-same (pref matrix #(0 2 1) #(1 0 1)) #(1 4 3))
    (ensure-same (pref vector #(3 1 2 0)) #(3 1 2 0))
    (ensure-error (pref vector #(1) #(0)))
    (ensure-error (pref matrix #(1 0) #(0)))))


(addtest (array-tests)
  filter-rows-test
  (let ((matrix (ia 4 3))
        (*lift-equality-test* #'equalp)
        (expected-result #2A((0 1 2) (6 7 8))))
    (ensure-same (filter-rows (lambda (vector) (evenp (aref vector 0))) matrix)
                 expected-result)
    (ensure-same (with-filter-rows matrix ; so much simpler, eh?
                     ((a 0))
                   (evenp a))
                 expected-result)))
