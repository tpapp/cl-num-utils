;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite statistics-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (statistics-tests)
  test-mean
  (ensure-same (mean (ia 5)) 2)
  (ensure-same (mean (ia 9)) 4))

(addtest (statistics-tests)
  test-variance
 (ensure-same (variance (ia 9)) 15/2)
 (ensure-same (variance (ia 20)) 35))

(addtest (statistics-tests)
  sse-off-center-test
  (let+ ((a (ia 9))
         (b (ia* 7 19))
         ((&flet sse2 (seq center)
            (sum seq :key (lambda (x) (expt (- x center) 2)))))
         (*lift-equality-test* #'==))
    (ensure-same (sse a 1.1) (sse2 a 1.1))
    (ensure-same (sse b 1.1) (sse2 b 1.1))
    (ensure-same (sse a pi) (sse2 a pi))))

(addtest (statistics-tests)
  test-array-mean
  (let+ ((v1 (ia 6))
         (v2 (e+ v1 3))
         (v3 (e+ v1 5))
         (vectors (vector v1 v2 v3))
         (vm (e+ v1 8/3))
         ((&flet v->a (v)
            (displace-array v '(2 3))))
         (am (v->a vm))
         (*lift-equality-test* #'==))
    (ensure-same (mean vectors) vm)
    (ensure-same (mean (map 'vector #'v->a vectors)) am)
    (ensure-error (mean (list v1 am)))))

;; (defun naive-weighted-variance (sample weights)
;;   "Calculate weighted variance (and mean, returned as the second value) using
;; the naive method."
;;   (let* ((sw (sum weights))
;;          (mean (/ (reduce #'+ (map 'vector #'* sample weights)) sw))
;;          (variance (/ (reduce #'+
;;                               (map 'vector 
;;                                    (lambda (s w) (* (expt (- s mean) 2) w))
;;                                    sample weights))
;;                       (1- sw))))
;;     (values variance mean)))

;; (addtest (statistics-tests)
;;   test-weighted
;;   (let ((s1 #(1 2 3))
;;         (w1 #(4 5 6))
;;         (*lift-equality-test* #'==)
;;         (s2 (random-vector 50 'double-float))
;;         (w2 (random-vector 50 'double-float)))
;;     (ensure-same (naive-weighted-variance s1 w1) (weighted-variance s1 w1))
;;     (ensure-same (naive-weighted-variance s2 w2) (weighted-variance s2 w2)
;;                  :test (lambda (x y)
;;                          (< (/ (abs (- x y))
;;                                (max 1 (abs x) (abs y)))
;;                             1d-5)))
;;     (ensure-same (second (multiple-value-list (weighted-variance s1 w1)))
;;                  (weighted-mean s1 w1))))

(defparameter *a* (let ((a (covariance-accumulator)))
                    (add a (cons 0 0))
                    (add a (cons 1 1))
                    (add a (cons 2 2))
                    a))

(addtest (statistics-tests)
  test-covariance
  (ensure-same (covariance-xy (ia 3) (ia 3)) (variance (ia 3)))
  (ensure-same (covariance-xy #(2 3 5) #(7 11 13)) (float 13/3 1d0)))

(addtest (statistics-tests)
  test-autocovariance
  (let+ ((n 200)
         (a (filled-array 200 (curry #'random 1d0)))
         ((&flet naive-cov (lag)
            (covariance-xy (subseq a 0 (- n lag)) (subseq a lag))))
         ((&values acv acc) (autocovariances a 3))
         (#(c1 c2 c3) acv)
         (v (variance acc))
         (#(r1 r2 r3) (autocorrelations acc))
         (*lift-equality-test* #'==))
    (ensure-same c1 (naive-cov 1))
    (ensure-same c2 (naive-cov 2))
    (ensure-same c3 (naive-cov 3))
    (ensure-same (variance a) v)
    (ensure-same r1 (/ c1 v))
    (ensure-same r2 (/ c2 v))
    (ensure-same r3 (/ c3 v))
    (ensure-same (autocorrelations a 3) (autocorrelations acc 3))
    (ensure-same (tally a) (tally acc))
    (ensure-same (mean a) (mean acc))
    (ensure-same (lags acc) 3)))

(addtest (statistics-tests)
  test-pool
  (let* ((n 100)
         (vector (filled-array (* 2 n) (curry #'random 1d0) 'double-float))
         (acc1 (sweep 'sse (subseq vector 0 n)))
         (acc2 (sweep 'sse (subseq vector n)))
         (acc (sweep 'sse vector))
         (acc-pooled (pool acc1 acc2))
         (*lift-equality-test* #'==))
    (ensure-same (tally acc) (tally acc-pooled))
    (ensure-same (mean acc) (mean acc-pooled))
    (ensure-same (variance acc) (variance acc-pooled))))

(addtest (statistics-tests)
  quantiles
  (let ((sample #(0.0 1.0))
        (quantiles (numseq 0 1 :length 11 :type 'double-float)))
    (ensure-same (map 'vector (curry #'quantile sample) quantiles)
                 quantiles)))

(addtest (statistics-tests)
  subranges
  (let+ (((&flet random-ranges (n &key (max 200) (order? t))
            (filled-array n (lambda ()
                              (let ((start (random max))
                                    (end (random max)))
                                (when (and order? (> start end))
                                  (rotatef start end))
                                (cons start end))))))
         ((&flet assemble-range (subranges index-list)
            (unless index-list
              (return-from assemble-range nil))
            (iter
              (with start)
              (with end)
              (for index :in index-list)
              (for subrange := (aref subranges index))
              (for previous-subrange :previous subrange :initially nil)
              (if (first-iteration-p)
                  (setf start (car subrange))
                  (assert (= (car subrange) (cdr previous-subrange))))
              (setf end (cdr subrange))
              (finally
               (return (cons start end)))))))
    (loop
      repeat 100000 do
     (let+ ((ranges (random-ranges 10 :order? nil))
            ((&values subranges index-lists) (subranges ranges)))
       (iter
         (for index-list :in-vector index-lists)
         (for range :in-vector ranges)
         (for assembled-range := (assemble-range subranges index-list))
         (for match? := (if assembled-range
                            (equal range assembled-range)
                            (>= (car range) (cdr range))))
         (unless match?
           (format *error-output* "mismatch: range ~A assembled to ~A"
                   range assembled-range))
         (ensure match?))))))
