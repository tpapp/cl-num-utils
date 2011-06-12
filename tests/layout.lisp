;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite layout-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (layout-tests)
  layout-test1
  (let+ ((layout (dictionary-layout
                  `((:A . ,(array-layout 3))
                    (:B . ,(atomic-layout))
                    (:C . ,(array-layout '(3 2))))))
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
    (ensure-error (layout-ref vector layout :c 1 0 4)))
  (let+ ((layout (atomic-dictionary-layout '(:a :b :c)))
         (vector (ivec (layout-length layout))))
    (ensure-same (layout-ref vector layout :a) 0)
    (ensure-same (layout-ref vector layout :c) 2)
    (ensure-error (layout-ref vector layout :d))))


