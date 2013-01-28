;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(defsuite statistics-tests (tests))

;;; testing cental moments

(defun precise-central-moments (sequence)
  "First 4 central moments, calculated using rationals, returned as four values, normalized by the length of the sequence.

Slow, but useful for testing as it does not suffer from approximation error."
  (let+ ((vector (map 'vector #'rational sequence))
         (n (length vector))
         (mean (/ (reduce #'+ vector) n))
         ((&flet central-m (degree)
            (/ (reduce #'+ vector
                       :key (lambda (v)
                              (expt (- v mean) degree)))
               n))))
    (values mean (central-m 2) (central-m 3) (central-m 4))))

(defun precise-weighted-central-moments (sequence weights)
  "First 4 weighted central moments, calculated using rationals, returned as four values, normalized by the length of the sequence.

Slow, but useful for testing as it does not suffer from approximation error."
  (assert (length= sequence weights))
  (let+ ((vector (map 'vector #'rational sequence))
         (weights (map 'vector #'rational weights))
         (w (reduce #'+ weights))
         (mean (/ (reduce #'+ (map 'vector #'* vector weights)) w))
         ((&flet central-m (degree)
            (/ (reduce #'+ (map 'vector (lambda (v w)
                                          (* w (expt (- v mean) degree)))
                                vector weights))
               w))))
    (values mean (central-m 2) (central-m 3) (central-m 4))))

;;; randomized tests

(defun random-floats (n mean &optional (element-type 'double-float))
  "Return a N-element vector of random floats (with given ELEMENT-TYPE).  A uniform random number from either [-1,0] or [0,3] (with equal probability) is added to MEAN, which ensures nonzero third and fourth central moments.  Higher abolute value of MEAN makes the calculation of higher central moments more ill-conditioned when using floats."
  (let ((mean (coerce mean element-type))
        (one (coerce 1 element-type)))
    (aops:generate* element-type
                    (lambda ()
                      (let ((v (random one)))
                        (if (zerop (random 2))
                            (- mean v)
                            (+ mean (* 3 v)))))
                    n)))

(defun random-weights (n range &optional (element-type 'double-float))
  "Random weights between (exp 1) and (exp (1+ range)), with given element-type."
  (aops:generate* element-type (let ((range (coerce range element-type)))
                                 (lambda ()
                                   (exp (1+ (random range)))))
                  n))

(defun test-moments (n mean &optional (element-type 'double-float))
  "Test that moments calculated precisely and with accumulators are equal."
  (let+ ((v (random-floats n mean element-type))
         ((&values m-p m2-p m3-p m4-p) (precise-central-moments v))
         ((&accessors-r/o mean central-m2 central-m3 central-m4)
          (central-sample-moments v :degree 4))
         (*num=-tolerance* 1e-8))
    (assert-equality #'num= mean m-p)
    (assert-equality #'num= central-m2 m2-p)
    (assert-equality #'num= central-m3 m3-p)
    (assert-equality #'num= central-m4 m4-p)))

(defun test-weighted-moments (n mean &key (weight-range 4) (element-type 'double-float))
  "Test that moments calculated precisely and with accumulators are equal."
  (let+ ((v (random-floats n mean element-type))
         (w (random-weights n weight-range element-type))
         ((&values m-p m2-p m3-p m4-p) (precise-weighted-central-moments v w))
         ((&accessors-r/o mean central-m2 central-m3 central-m4)
          (central-sample-moments v :degree 4 :weights w))
         (*num=-tolerance* 1e-8))
    (assert-equality #'num= mean m-p)
    (assert-equality #'num= central-m2 m2-p)
    (assert-equality #'num= central-m3 m3-p)
    (assert-equality #'num= central-m4 m4-p)))

(deftest central-moments-test1 (statistics-tests)
  (test-moments 1000 0)
  (test-moments 1000 10)
  (test-moments 1000 100)
  (test-moments 1000 1000000))

(deftest central-moments-test2 (statistics-tests)
  (test-weighted-moments 10 0)
  (test-weighted-moments 1000 10)
  (test-weighted-moments 1000 100)
  (test-weighted-moments 1000 1000000))

(defun test-pooled-moments (n mean &optional (element-type 'double-float))
  (let* ((v (random-floats (* 2 n) mean element-type))
         (v1 (subseq v 0 n))
         (v2 (subseq v n nil))
         (m (central-sample-moments v :degree 4))
         (m1 (central-sample-moments v1 :degree 4))
         (m2 (central-sample-moments v2 :degree 4))
         (m12 (pool m1 m2))
         (*num=-tolerance* 1e-8))
    (assert-equality #'num= (mean m) (mean m12))))

(deftest pooled-moments-test1 (statistics-tests)
  (test-pooled-moments 1000 0)
  (test-pooled-moments 1000 10)
  (test-pooled-moments 1000 100)
  (test-pooled-moments 1000 1000000))

;; (addtest (statistics-tests)
;;   test-ratio
;;   (let+ ((v (map1 #'bit-to-boolean #*000011111))
;;          ((&values ratio acc) (sample-ratio v)))
;;     (ensure-same ratio 5/9)
;;     (ensure-same (tally acc) 9)))

(deftest test-invalid-types (statistics-tests)
  (assert-condition error (add (central-sample-moments) 'foo))
  (assert-condition error (add (central-sample-moments) #(1 2 3)))
  ;; (ensure-error (add (autocovariance-accumulator 9) 'foo))
  ;; (ensure-error (add (autocovariance-accumulator 9) #(1 2 3)))
  )

(deftest test-mean (statistics-tests)
  (assert-equalp 2 (mean (iota 5)))
  (assert-equalp 4 (mean (iota 9))))

(deftest test-variance (statistics-tests)
 (assert-equalp 15/2 (variance (iota 9)))
 (assert-equalp 35 (variance (iota 20))))

(deftest pooled-moments-test (statistics-tests)
  (let ((v #(62.944711253834164d0 81.15843153081796d0 25.397393118645773d0
             82.67519197788647d0 26.471834961609876d0 19.50812790113414d0
             55.69965251750717d0 9.376334465151004d0 91.50142635930303d0
             92.97771145772332d0 31.522629341026608d0 94.11859332177082d0
             91.43334482781893d0 97.07515698488776d0 60.05604715628141d0
             28.377247312878072d0 84.35221790928993d0 83.14710996278352d0
             58.44153198534443d0 91.89848934771322d0)))
    (assert-equality #'num=  (central-sample-moments v :degree 4)
        (pool (central-sample-moments (subseq v 0 7) :degree 4)
              (central-sample-moments (subseq v 7) :degree 4)))))

;; (addtest (statistics-tests)
;;   sse-off-center-test
;;   (let+ ((a (ia 9))
;;          (b (ia* 7 19))
;;          ((&flet sse2 (seq center)
;;             (sum seq :key (lambda (x) (expt (- x center) 2)))))
;;          (*lift-equality-test* #'==))
;;     (assert-equality  (sse a 1.1) (sse2 a 1.1))
;;     (assert-equality  (sse b 1.1) (sse2 b 1.1))
;;     (assert-equality  (sse a pi) (sse2 a pi))))

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
;;     (assert-equality  (mean vectors) vm)
;;     (assert-equality  (mean (map 'vector #'v->a vectors)) am)
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
;;     (assert-equality  (naive-weighted-variance s1 w1) (weighted-variance s1 w1))
;;     (assert-equality  (naive-weighted-variance s2 w2) (weighted-variance s2 w2)
;;                  :test (lambda (x y)
;;                          (< (/ (abs (- x y))
;;                                (max 1 (abs x) (abs y)))
;;                             1d-5)))
;;     (assert-equality  (second (multiple-value-list (weighted-variance s1 w1)))
;;                  (weighted-mean s1 w1))))

;; (defparameter *a* (let ((a (covariance-accumulator)))
;;                     (add a (cons 0 0))
;;                     (add a (cons 1 1))
;;                     (add a (cons 2 2))
;;                     a))

;; (addtest (statistics-tests)
;;   test-covariance
;;   (assert-equality  (covariance-xy (ia 3) (ia 3)) (variance (ia 3)))
;;   (assert-equality  (covariance-xy #(2 3 5) #(7 11 13)) (float 13/3 1d0)))

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
;;     (assert-equality  c1 (cov 1))
;;     (assert-equality  c2 (cov 2))
;;     (assert-equality  c3 (cov 3))
;;     (assert-equality  r1 (corr 1))
;;     (assert-equality  r2 (corr 2))
;;     (assert-equality  r3 (corr 3))
;;     (assert-equality  (autocorrelations a 3) (autocorrelations acc 3))
;;     (assert-equality  (lags acc) 3)))

;; (addtest (statistics-tests)
;;   test-pool
;;   (let* ((n 100)
;;          (vector (generate-array (* 2 n) (curry #'random 1d0) 'double-float))
;;          (acc1 (sweep 'sse (subseq vector 0 n)))
;;          (acc2 (sweep 'sse (subseq vector n)))
;;          (acc (sweep 'sse vector))
;;          (acc-pooled (pool acc1 acc2))
;;          (*lift-equality-test* #'==))
;;     (assert-equality  acc acc-pooled)))

(deftest quantiles (statistics-tests)
  (let ((sample #(0.0 1.0))
        (quantiles (numseq 0 1 :length 11 :type 'double-float)))
    (assert-equalp #(0.0 0.0 0.0 0.1 0.3 0.5 0.7 0.9 1.0 1.0 1.0)
        (map 'vector (curry #'quantile sample) quantiles))))

(ensure-sorted-reals #(0.0 1.0))

(deftest quantile-probabilities (statistics-tests)
  (let* ((n 10)
         (sample (sort (aops:generate (lambda () (random (* n 2))) n) #'<))
         (empirical-quantile-probabilities n))
    (assert-equalp sample
        (quantiles sample (empirical-quantile-probabilities
                                       (length sample))))))

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
;;     (assert-equality  (limits result) `((0 . ,end)))
;;     (assert-equality  offset '(0))
;;     (assert-equality  array1 array2)
;;     (loop for i below end do
;;       (assert-equality  (ref result i) (aref array1 i)))))

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
;;     (assert-equality  (subranges ranges :shadow-ranges shadow-ranges)
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
;;     ;; (assert-equality  (total-frequency histogram) 5)
;;     (assert-equality  (ref histogram 0) 2)
;;     (assert-equality  (ref histogram 1) 3)
;;     (assert-equality  (ref histogram -7) 0)
;;     (ensure-error (add histogram '(0 0)))
;;     (assert-equality  (limits histogram) '((0 . 2)))
;;     (assert-equality  (location-limits histogram) (list (interval -1 5)))))

;; (addtest (statistics-tests)
;;   histogram-test2
;;   (let ((histogram (histogram1 '(1 2 3 2 3 3) (integer-bins))))
;;     (assert-equality  (ref histogram 1) 1)
;;     (assert-equality  (ref histogram 2) 2)
;;     (assert-equality  (ref histogram 3) 3)
;;     (assert-equality  (ref histogram 7) 0)
;;     ;; (assert-equality  (total-frequency histogram) 6)
;;     ;; (assert-equality  (relative-frequency histogram 1) 1/6)
;;     ;; (assert-equality  (relative-frequency histogram 2) 2/6)
;;     ;; (assert-equality  (relative-frequency histogram 3) 3/6)
;;     ;; (assert-equality  (relative-frequency histogram 7) 0)
;;     (assert-equality  (limits histogram) '((1 . 4)))))
