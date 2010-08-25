;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defun sequence= (a b)
  "Test equality of A and B elementwise (also tests that elements are
of the same type)."
  (and (if (and (vectorp a) (vectorp b))
           (equal (array-element-type a)
                  (array-element-type b))
           (and (listp a) (listp b)))
       (every #'eql a b)))

(deftestsuite seq-tests (cl-num-utils-tests)
  ()
  (:equality-test #'sequence=))

(addtest (seq-tests)
  vector*
  (let* ((element-type 'double-float)
         (vector (vector* element-type 3 5 7)))
    (ensure-same (type-of vector) 
                 `(simple-array ,(upgraded-array-element-type element-type) (3))
                 :test #'equal)
    (ensure (every #'eql vector #(3d0 5d0 7d0)))))

(addtest (seq-tests)
  sequence=
  (ensure-same (vector* 'double-float 1 2 3)
               (vector* 'double-float 1 2.0 3d0))
  (ensure-same '(1 2 3) '(1 2 3))
  (ensure-different '(1d0 2 3) '(1 2 3))
  (ensure-different '(1 2 3) (vector* 'fixnum 1 2 3)))

(addtest (seq-tests)
  seq
  ;; missing :LENGTH (default :BY)
  (ensure-same (numseq 0 5) 
               (vector* 'fixnum 0 1 2 3 4 5))
  ;; missing :TO
  (ensure-same (numseq 1 nil :by 1/2 :length 3 :type 'list)
               '(1 3/2 2))
  ;; missing :FROM
  (ensure-same (numseq nil 9 :by 1d0 :length 4)
               (vector* 'double-float 6d0 7d0 8d0 9d0))
  ;; missing :LENGTH, automatic direction for :by
  (ensure-same (numseq 9 8 :by 0.5 :type 'list)
               '(9.0 8.5 8.0))
  (ensure-same (numseq 9 8 :by -0.5 :type 'list)
               '(9.0 8.5 8.0)))

(addtest (seq-tests)
  vector-satisfies?
  (ensure (vector-satisfies? #(1 2 3) #'<))
  (ensure (not (vector-satisfies? #(1 1 2) #'<)))
  (ensure (not (vector-satisfies? #(3 2 1) #'<=)))
  (ensure (vector-satisfies? #(1) #'<))
  (ensure (vector-satisfies? #() #'<))
  (ensure-error (vector-satisfies? 'not-a-vector #'<))
  (ensure-error (vector-satisfies? '(not a vector) #'<)))

(addtest (seq-tests)
  rep
  (ensure-same (rep '(1 2 3) 4 2)
               '(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3))
  (ensure-same (rep #(1 2 3) 4 2)
               #(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3)))

(addtest (seq-tests)
  concat-test
  (ensure-same (concat #(1 2 3) #(4 5 6) (list  7) '(8 9 10))
               (numseq 1 10 :type t) :test #'equalp))

