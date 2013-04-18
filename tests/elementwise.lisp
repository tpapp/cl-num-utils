;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite elementwise-tests (tests))

(deftest elementwise-float-contagion (elementwise-tests)
  (flet ((compare (type &rest objects)
           (type= (apply #'cl-num-utils.elementwise::elementwise-float-contagion
                         objects) type)))
    (assert-true (compare 'double-float 1d0) 0)
    (assert-true (compare 'real 0 1))))

(deftest e-operations-tests (elementwise-tests)
  (let+ (((&flet arr (dimensions element-type &rest elements)
            (aprog1 (make-array dimensions :element-type element-type)
              (assert (length= elements (array-total-size it)))
              (loop for index from 0
                    for element in elements
                    do (setf (row-major-aref it index)
                             (coerce element element-type))))))
         (a (arr '(2 3) 'double-float
                 1 2 3
                 4 5 6))
         (b (arr '(2 3) 'single-float
                 2 3 5
                 7 11 13)))
    (assert-equalp (e+ a b) (arr '(2 3) 'double-float
                                 3 5 8
                                 11 16 19))
    (assert-equalp (e* a 2s0) (arr '(2 3) 'double-float
                                   2 4 6
                                   8 10 12))
    (assert-equalp (e+ a 2 b) (e+ (e+ a b) 2))
    (assert-equalp (e+ a a) (e* a 2))
    ;; (ensure-error (e/ a 0))             ; division by 0
    (assert-condition error (e+ a       ; dimension incompatibility
                                (arr '(1 1) 'double-float 2)))
    (assert-equalp (e+ a) (e+ a 0))
    (assert-equalp (e* a) (e* a 1))
    (assert-equalp (e- a) (e- 0d0 a))
    (assert-equalp (e/ a) (e/ 1d0 a))
    (assert-equality #'num= #(1.0) (elog #(10) 10))
    (assert-equality #'num= a (eexp (elog a)))))

;; (deftest (elementwise-tests)
;;   stack-tests
;;   (let ((a (array* '(2 3) t
;;                    1 2 3
;;                    4 5 6))
;;         (b (array* '(2 2) t
;;                    3 5
;;                    7 9))
;;         (*lift-equality-test* #'equalp))
;;     (assert-equalp (stack 'double-float :h a b)
;;                  (array* '(2 5) 'double-float
;;                          1 2 3 3 5
;;                          4 5 6 7 9))
;;     (assert-equalp (stack t :v (transpose a) b)
;;                  #2A((1 4)
;;                      (2 5)
;;                      (3 6)
;;                      (3 5)
;;                      (7 9)))
;;     (assert-equalp (stack 'fixnum :v a #(7 8 9) 10)
;;                  (array* '(4 3) 'fixnum
;;                           1 2 3
;;                           4 5 6
;;                           7 8 9
;;                           10 10 10))
;;     (assert-equalp (stack t :h b #(1 2) b 9 b)
;;                  (array* '(2 8) t
;;                          3 5 1 3 5 9 3 5
;;                          7 9 2 7 9 9 7 9))
;;     (assert-equalp (stack t :h
;;                         (vector* 'double-float 1d0 2d0)
;;                         (vector* 'double-float 3d0 4d0))
;;                  (array* '(2 2) 'double-float
;;                          1 3
;;                          2 4))
;;     (assert-equalp (stack 'double-float :h 1.0d0 #()) ; empty array
;;                  (array* '(0 2) 'double-float))))

;; (deftest (elementwise-tests)
;;   concat-test
;;   (assert-equalp (concat t #(1 2 3) #(4 5 6) (list  7) '(8 9 10))
;;                (numseq 1 10 :type t) :test #'equalp))
