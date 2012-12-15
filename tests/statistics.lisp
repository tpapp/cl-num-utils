;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite statistics-tests (cl-num-utils-tests)
  ()
  (:equality-test #'==))

;; (addtest (statistics-tests)
;;   test-ratio
;;   (let+ ((v (map1 #'bit-to-boolean #*000011111))
;;          ((&values ratio acc) (sample-ratio v)))
;;     (ensure-same ratio 5/9)
;;     (ensure-same (tally acc) 9)))

(addtest (statistics-tests)
  test-invalid-types
  (ensure-error (add (central-sample-moments) 'foo))
  (ensure-error (add (central-sample-moments) #(1 2 3)))
  ;; (ensure-error (add (autocovariance-accumulator 9) 'foo))
  ;; (ensure-error (add (autocovariance-accumulator 9) #(1 2 3)))
  )

(addtest (statistics-tests)
  test-mean
  (ensure-same (mean (ia 5)) 2)
  (ensure-same (mean (ia 9)) 4))

(addtest (statistics-tests)
  test-variance
 (ensure-same (variance (ia 9)) 15/2)
 (ensure-same (variance (ia 20)) 35))

(defun naive-two-pass-moments (sample)
  "Return the mean and the sum deviation from the mean to the 2nd, 3rd and 4th
powers; as a vector of double floats.  For testing only, subject to numerical
instabilities."
  (let+ ((sample (map '(simple-array double-float (*))
                      (lambda (y) (coerce y 'double-float))
                      sample))
         (n (length sample))
         (mean (/ (reduce #'+ sample) n))
         ((&flet mk (k)
            (/ (reduce #'+ sample :key (lambda (y)
                                         (expt (- y mean) k)))
               n))))
    (vector mean (mk 2) (mk 3) (mk 4))))

(defmacro ensure-same-moments (sample)
  (with-unique-names (moments m1 m2 m3 m4)
    (once-only (sample)
      `(let+ ((#(,m1 ,m2 ,m3 ,m4) (naive-two-pass-moments ,sample))
              (,moments (central-sample-moments ,sample 4))
              (*lift-equality-test* #'==))
         (ensure-same ,m1 (mean ,moments))
         (ensure-same ,m2 (central-m2 ,moments))
         (ensure-same ,m3 (central-m3 ,moments))
         (ensure-same ,m4 (central-m4 ,moments))))))

(addtest (statistics-tests)
  central-sample-moments-test
  (ensure-same-moments #(1 2 3 7 9))
  (ensure-same-moments #(101 107 227 119 72)))

;; (addtest (statistics-tests)
;;   sse-off-center-test
;;   (let+ ((a (ia 9))
;;          (b (ia* 7 19))
;;          ((&flet sse2 (seq center)
;;             (sum seq :key (lambda (x) (expt (- x center) 2)))))
;;          (*lift-equality-test* #'==))
;;     (ensure-same (sse a 1.1) (sse2 a 1.1))
;;     (ensure-same (sse b 1.1) (sse2 b 1.1))
;;     (ensure-same (sse a pi) (sse2 a pi))))

;; (addtest (statistics-tests)
;;   test-array-mean
;;   (let+ ((v1 (ia 6))
;;          (v2 (e+ v1 3))
;;          (v3 (e+ v1 5))
;;          (vectors (vector v1 v2 v3))
;;          (vm (e+ v1 8/3))
;;          ((&flet v->a (v)
;;             (displace-array v '(2 3))))
;;          (am (v->a vm))
;;          (*lift-equality-test* #'==))
;;     (ensure-same (mean vectors) vm)
;;     (ensure-same (mean (map 'vector #'v->a vectors)) am)
;;     (ensure-error (mean (list v1 am)))))

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

;; (defparameter *a* (let ((a (covariance-accumulator)))
;;                     (add a (cons 0 0))
;;                     (add a (cons 1 1))
;;                     (add a (cons 2 2))
;;                     a))

;; (addtest (statistics-tests)
;;   test-covariance
;;   (ensure-same (covariance-xy (ia 3) (ia 3)) (variance (ia 3)))
;;   (ensure-same (covariance-xy #(2 3 5) #(7 11 13)) (float 13/3 1d0)))

;; (addtest (statistics-tests)
;;   test-autocovariance
;;   (let+ ((n 200)
;;          (a (generate-array 200 (curry #'random 1d0)))
;;          ((&flet lagged (function lag)
;;             (funcall function (subseq a 0 (- n lag)) (subseq a lag))))
;;          ((&flet cov (lag) (lagged #'covariance-xy lag)))
;;          ((&flet corr (lag) (lagged #'correlation-xy lag)))
;;          ((&values acv acc) (autocovariances a 3))
;;          (#(c1 c2 c3) acv)
;;          (#(r1 r2 r3) (autocorrelations acc))
;;          (*lift-equality-test* #'==))
;;     (ensure-same c1 (cov 1))
;;     (ensure-same c2 (cov 2))
;;     (ensure-same c3 (cov 3))
;;     (ensure-same r1 (corr 1))
;;     (ensure-same r2 (corr 2))
;;     (ensure-same r3 (corr 3))
;;     (ensure-same (autocorrelations a 3) (autocorrelations acc 3))
;;     (ensure-same (lags acc) 3)))

;; (addtest (statistics-tests)
;;   test-pool
;;   (let* ((n 100)
;;          (vector (generate-array (* 2 n) (curry #'random 1d0) 'double-float))
;;          (acc1 (sweep 'sse (subseq vector 0 n)))
;;          (acc2 (sweep 'sse (subseq vector n)))
;;          (acc (sweep 'sse vector))
;;          (acc-pooled (pool acc1 acc2))
;;          (*lift-equality-test* #'==))
;;     (ensure-same acc acc-pooled)))

;; (addtest (statistics-tests)
;;   quantiles
;;   (let ((sample #(0.0 1.0))
;;         (quantiles (numseq 0 1 :length 11 :type 'double-float)))
;;     (ensure-same (map 'vector (curry #'quantile sample) quantiles)
;;                  #(0.0 0.0 0.0 0.1 0.3 0.5 0.7 0.9 1.0 1.0 1.0))))

;; (addtest (statistics-tests)
;;   quantile-probabilities
;;   (let* ((n 10)
;;          (sample (sort (generate-array n (lambda () (random (* n 2)))) #'<))
;;          (empirical-quantile-probabilities n))
;;     (ensure-same (quantiles sample (empirical-quantile-probabilities
;;                                     (length sample)))
;;                  sample)))

;; (addtest (statistics-tests)
;;   (let+ ((end 5)
;;          (index 0)                      ; to make sure we have 1 of each
;;          (pairs (generate-array 100
;;                               (lambda ()
;;                                 (prog1 (at (random 100d0)
;;                                            (if (< index end)
;;                                                index
;;                                                (random end)))
;;                                   (incf index)))))
;;          (acc #'mean-sse-accumulator)
;;          (result (sweep (sparse-accumulator-array 1 acc) pairs))
;;          (*lift-equality-test* #'==)
;;          ((&flet sparse-acc (pairs accumulator &rest s)
;;             "For testing."
;;             (map nil (lambda+ ((&structure at- object subscripts))
;;                        (when (equal s subscripts)
;;                          (add accumulator object)))
;;                  pairs)
;;             (when (plusp (tally accumulator))
;;               accumulator)))
;;          (array1 (iter
;;                    (for i below end)
;;                    (collect (sparse-acc pairs (funcall acc) i)
;;                      :result-type vector)))
;;          ((&values array2 offset) (as-array result)))
;;     (ensure-same (limits result) `((0 . ,end)))
;;     (ensure-same offset '(0))
;;     (ensure-same array1 array2)
;;     (loop for i below end do
;;       (ensure-same (ref result i) (aref array1 i)))))

;; (addtest (statistics-tests)
;;   subranges
;;   (let+ (((&flet random-ranges (n &key (max 200) (order? t))
;;             (generate-array n (lambda ()
;;                                 (let ((start (random max))
;;                                       (end (random max)))
;;                                   (when (and order? (> start end))
;;                                     (rotatef start end))
;;                                   (cons start end))))))
;;          ((&flet assemble-range (subranges index-list)
;;             (unless index-list
;;               (return-from assemble-range nil))
;;             (iter
;;               (with start)
;;               (with end)
;;               (for index :in index-list)
;;               (for subrange := (aref subranges index))
;;               (for previous-subrange :previous subrange :initially nil)
;;               (if (first-iteration-p)
;;                   (setf start (car subrange))
;;                   (assert (= (car subrange) (cdr previous-subrange))))
;;               (setf end (cdr subrange))
;;               (finally
;;                (return (cons start end)))))))
;;     (loop
;;       repeat 100000 do
;;         (let+ ((ranges (random-ranges 10 :order? nil))
;;                ((&values subranges index-lists) (subranges ranges)))
;;           (iter
;;             (for index-list :in-vector index-lists)
;;             (for range :in-vector ranges)
;;             (for assembled-range := (assemble-range subranges index-list))
;;             (for match? := (if assembled-range
;;                                (equal range assembled-range)
;;                                (>= (car range) (cdr range))))
;;             (unless match?
;;               (format *error-output* "mismatch: range ~A assembled to ~A"
;;                       range assembled-range))
;;             (ensure match?))))))

;; (addtest (statistics-tests)
;;   (let ((ranges #((20 . 40) (60 . 120) (100 . 180)))
;;         (shadow-ranges '((0 . 200)))
;;         (*lift-equality-test* #'equalp))
;;     (ensure-same (subranges ranges :shadow-ranges shadow-ranges)
;;                  (values #((0 . 20) (20 . 40) (40 . 60) (60 . 100) (100 . 120)
;;                            (120 . 180) (180 . 200))
;;                          #((1) (3 4) (4 5))))))

;; (addtest (statistics-tests)
;;   histogram-test
;;   (let ((histogram (histogram-accumulator (even-bins 2 -1)))
;;         (*lift-equality-test* #'==))
;;     (add histogram 0)
;;     (add histogram 0.5)
;;     (loop repeat 3 do (add histogram 1.5))
;;     ;; (ensure-same (total-frequency histogram) 5)
;;     (ensure-same (ref histogram 0) 2)
;;     (ensure-same (ref histogram 1) 3)
;;     (ensure-same (ref histogram -7) 0)
;;     (ensure-error (add histogram '(0 0)))
;;     (ensure-same (limits histogram) '((0 . 2)))
;;     (ensure-same (location-limits histogram) (list (interval -1 5)))))

;; (addtest (statistics-tests)
;;   histogram-test2
;;   (let ((histogram (histogram1 '(1 2 3 2 3 3) (integer-bins))))
;;     (ensure-same (ref histogram 1) 1)
;;     (ensure-same (ref histogram 2) 2)
;;     (ensure-same (ref histogram 3) 3)
;;     (ensure-same (ref histogram 7) 0)
;;     ;; (ensure-same (total-frequency histogram) 6)
;;     ;; (ensure-same (relative-frequency histogram 1) 1/6)
;;     ;; (ensure-same (relative-frequency histogram 2) 2/6)
;;     ;; (ensure-same (relative-frequency histogram 3) 3/6)
;;     ;; (ensure-same (relative-frequency histogram 7) 0)
;;     (ensure-same (limits histogram) '((1 . 4)))))
