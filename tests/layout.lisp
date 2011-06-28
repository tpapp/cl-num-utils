;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite layout-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (layout-tests)
  layout-test1
  (let+ ((layout (dictionary-layout
                  `((:A . ,(array-layout 3))
                    (:B . nil)
                    (:C . ,(array-layout 3 2)))))
         (vector (ivec (layout-length layout)))
         (*lift-equality-test* #'equalp))
    (ensure-same (layout-ref vector layout :a)
                 #(0 1 2))
    (ensure-same (layout-ref vector layout :a 1)
                 1)
    (ensure-error (layout-ref vector layout :a 1 3))

    (ensure-same (layout-ref vector layout :b) 3)
    (ensure-error (layout-ref vector layout :b 3) 3)
    (ensure-same (layout-ref vector layout :c) #2A((4 5)
                                                   (6 7)
                                                   (8 9)))
    (ensure-same (layout-ref vector layout :c 1) #(6 7))
    (ensure-same (layout-ref vector layout :c 1 0) 6)
    (ensure-error (layout-ref vector layout :c 1 0 4))))

(addtest (layout-tests)
  layout-test2
  (let+ ((layout (atomic-dictionary-layout '(:a :b :c)))
         (vector (ivec (layout-length layout))))
    (ensure-same (layout-ref vector layout :a) 0)
    (ensure-same (layout-ref vector layout :c) 2)
    (ensure-error (layout-ref vector layout :d))))

(addtest (layout-tests)
  layout-position-tests
  (let+ ((a (array-layout 2 4 3))
         (s (shifted-vector-layout 3 10))
         (d (dictionary-layout `((:f . nil) (:a . ,a) (:n . nil) (:s . ,s))))
         (*lift-equality-test* #'equalp))
    ;; array layout
    (ensure-same (layout-position a)
                 (values (cons 0 24) a))
    (ensure-same (layout-position a 1)
                 (values (cons 12 24) (array-layout 4 3)))
    (ensure-same (layout-position a 1 2)
                 (values (cons 18 21) (array-layout 3)))
    (ensure-same (layout-position a 1 2)
                 (values (cons 18 21) (array-layout 3)))
    (ensure-same (layout-position a 1 2 2)
                 (values 20 nil))
    (ensure-error (layout-position a 100))
    (ensure-error (layout-position a 0 0 0 0))
    ;; shifted vector
    (ensure-same (layout-position s) (values (cons 0 7) s))
    (ensure-same (layout-position s 4) (values 1 nil))
    (ensure-error (layout-position s 10))
    (ensure-error (layout-position s -10))
    (ensure-error (layout-position s 3 9))
    ;; atomic
    (ensure-same (layout-position nil) (values 0 nil))
    (ensure-error (layout-position nil 0))
    ;; dictionary
    (ensure-same (layout-position d :f) (values 0 nil))
    (ensure-error (layout-position d :f 2))
    (ensure-same (layout-position d :a) (values (cons 1 25) a))
    (ensure-same (layout-position d :a 1 2)
                 (values (cons 19 22) (array-layout 3)))
    (ensure-same (layout-position d :n) (values 25 nil))
    (ensure-same (layout-position d :s) (values (cons 26 33) s))
    (ensure-same (layout-position d :s 4) (values 27 nil))))
