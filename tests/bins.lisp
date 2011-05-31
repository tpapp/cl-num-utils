;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite bins-tests (cl-num-utils-tests)
  ())

(defmacro with-check-bin-index ((bins) &body body)
  "Within BODY, (CHECK-BIN-INDEX VALUE INDEX) will check that both
BIN-INDEX and BIN-FUNCTION map VALUE to INDEX."
  (once-only (bins)
    `(macrolet ((check-bin-index (value index)
                  `(ensure-same (bin-index ,',bins ,value) ,index)))
       ,@body)))

(addtest (bins-tests)
  even-bins
  (let* ((width 2)
         (offset 1)
         (bins (even-bins width offset))
         (index-start -5)
         (left-start (+ (* index-start width) offset)))
    (with-check-bin-index (bins)
      (iter
        (for index :from index-start :to (* 2 (abs index-start)))
        (for left :from left-start :by width)
        (for middle :from (+ left-start 0.001) :by width)
        (for right :from (+ left-start width) :by width)
        (check-bin-index left index)
        (check-bin-index middle index)
        (check-bin-index right (1+ index))))))

;; (addtest (bins-tests)
;;   irregular-bins
;;   (let* ((bins (irregular-bins #(1 2 3 4))))
;;     (with-check-bin-index (bins)
;;       (check-bin-index 1 0)
;;       (check-bin-index 1.5 0)
;;       (check-bin-index 2 1))
;;     (ensure-error (bin-value bins 0))
;;     (ensure-error (bin-value bins 4))))

(addtest (bins-tests)
  histogram-test
  (let ((histogram (make-hashed-histogram (even-bins 2 -1))))
    (add-observation histogram 1 0)
    (add-observation histogram 1 0.5)
    (add-observation histogram 3 1.5)
    (ensure-same (total-frequency histogram) 5)
    (ensure-same (frequency histogram 0) 2)
    (ensure-same (frequency histogram 1) 3)
    (ensure-same (frequency histogram -7) 0)
    (ensure-error (add-observation histogram 1 0 0))))

(addtest (bins-tests)
  histogram-test2
  (let ((histogram (histogram-from-sequence '(1 2 3 2 3 3)
                                            (integer-bins))))
    (ensure-same (frequency histogram 1) 1)
    (ensure-same (frequency histogram 2) 2)
    (ensure-same (frequency histogram 3) 3)
    (ensure-same (frequency histogram 7) 0)
    (ensure-same (total-frequency histogram) 6)
    (ensure-same (relative-frequency histogram 1) 1/6)
    (ensure-same (relative-frequency histogram 2) 2/6)
    (ensure-same (relative-frequency histogram 3) 3/6)
    (ensure-same (relative-frequency histogram 7) 0)
    (ensure-same (subscript-limit histogram 0) (cons 1 4))
    (ensure-error (subscript-limit histogram 1))))

(addtest (bins-tests)
  binary-search
  (flet ((test-binary-search (n &key (max n))
           "Test fixnum binary search by generating N random elements below
MAX, then finding a random number."
           (let* ((vector (sort
                           (remove-duplicates (filled-array n (curry #'random max)))
                           #'<=))
                  (value (random max))
                  (index (position value vector)) ; the hard way
                  (result (binary-search vector value)))
             (cond
               ((not index)
                (assert (not result) ()
                        "~A mistakenly found in ~A at index ~A"
                        value vector result)
                t)
               ((not result)
                (error "~A not found in ~A" value vector))
               ((/= index result)
                (error "~A found in ~A at location ~A instead of ~A"
                       value vector result index))
               (t t)))))
    (loop repeat 10000 do
      (ensure (test-binary-search (1+ (random 40)))))))
