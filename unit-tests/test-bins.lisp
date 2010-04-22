;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite bins-tests (cl-num-utils-tests)
  ())

(defmacro with-check-bin-index ((bins bin-function) &body body)
  "Within BODY, (CHECK-BIN-INDEX VALUE INDEX) will check that both
BIN-INDEX and BIN-FUNCTION map VALUE to INDEX."
  (once-only (bins bin-function)
    `(macrolet ((check-bin-index (value index)
                  (once-only (value index)
                    `(progn
                       (ensure-same (bin-index ,',bins ,value) ,index)
                       (ensure-same (funcall ,',bin-function ,value) ,index)))))
       ,@body)))

(addtest (bins-tests)
  evenly-distributed-bins
  (let* ((bin-width 2)
         (offset 1)
         (b (make-instance 'evenly-distributed-bins :bin-width bin-width :offset offset))
         (index-start -5)
         (left-start (- (* index-start bin-width) offset)))
    (ensure-same (bin-range b) '(nil nil))
    (with-check-bin-index (b (bin-function b))
      (iter
        (for index :from index-start :to (* 2 (abs index-start)))
        (for left :from left-start :by bin-width)
        (for middle :from (+ left-start 0.001) :by bin-width)
        (for right :from (+ left-start bin-width) :by bin-width)
        (check-bin-index left index)
        (check-bin-index middle index)
        (check-bin-index right (1+ index))))))

(addtest (bins-tests)
  irregular-bins
  (let* ((b (make-instance 'irregular-bins :breaks #(1 2 3 4)))
         (b-f (bin-function b)))
    (with-check-bin-index (b b-f)
      (check-bin-index 1 0)
      (check-bin-index 1.5 0)
      (check-bin-index 2 1))
    (ensure-error (bin-index b 0))
    (ensure-error (funcall b-f 0))
    (ensure-error (bin-index b 4))
    (ensure-error (funcall b-f 4))))
