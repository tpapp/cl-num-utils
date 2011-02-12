;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite bins-tests (cl-num-utils-tests)
  ())

(defmacro with-check-bin-index ((bin-function) &body body)
  "Within BODY, (CHECK-BIN-INDEX VALUE INDEX) will check that both
BIN-INDEX and BIN-FUNCTION map VALUE to INDEX."
  (once-only (bin-function)
    `(macrolet ((check-bin-index (value index)
                  `(ensure-same (funcall ,',bin-function ,value) ,index)))
       ,@body)))

(addtest (bins-tests)
  even-bins
  (let* ((width 2)
         (offset 1)
         (bins (even-bins :width width :offset offset))
         (index-start -5)
         (left-start (- (* index-start width) offset)))
    (with-check-bin-index (bins)
      (iter
        (for index :from index-start :to (* 2 (abs index-start)))
        (for left :from left-start :by width)
        (for middle :from (+ left-start 0.001) :by width)
        (for right :from (+ left-start width) :by width)
        (check-bin-index left index)
        (check-bin-index middle index)
        (check-bin-index right (1+ index))))))

(addtest (bins-tests)
  irregular-bins
  (let* ((bins (irregular-bins #(1 2 3 4))))
    (with-check-bin-index (bins)
      (check-bin-index 1 0)
      (check-bin-index 1.5 0)
      (check-bin-index 2 1))
    (ensure-error (funcall bins 0))
    (ensure-error (funcall bins 4))))
