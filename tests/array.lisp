;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite array-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (array-tests)
  matrix-type-tests
  (let ((m (ia 3 4)))
    (ensure (typep m 'matrix))
    (ensure (typep m '(matrix *)))
    (ensure (typep m '(matrix t)))
    (ensure (typep m '(matrix t 3)))
    (ensure (typep m '(matrix t * 4)))
    (ensure (typep m '(matrix t 3 4)))
    (ensure (not (typep m '(matrix * 2))))))

(addtest (array-tests)
  diagonal
  (let ((a1 (ia 2 2))
        (a2 (ia 3 2))
        (a3 (ia 2 3))
        (*lift-equality-test* #'==))
    (ensure-same (diagonal a1) (vector 0 3))
    (ensure-same (diagonal a2) (vector 0 3))
    (ensure-same (diagonal a3) (vector 0 4))))

(addtest (array-tests)
  (flet ((fill-in-dimensions (dimensions size)
           (clnu::fill-in-dimensions dimensions size)))
    (ensure-same (fill-in-dimensions '(1 2 3) 6) '(1 2 3))
    (ensure-same (fill-in-dimensions '(1 t 3) 6) '(1 2 3))
    (ensure-same (fill-in-dimensions '(1 t 3) 0) '(1 0 3))
    (ensure-same (fill-in-dimensions 6 6) '(6))
    (ensure-same (fill-in-dimensions t 6) '(6))
    (ensure-error (fill-in-dimensions '(1 t t 3) 6))
    (ensure-error (fill-in-dimensions '(1 t 0 3) 6))))

(addtest (array-tests)
  reshape
  (let ((a (ia 3 4))
        (a-reshaped-rm #2A((0 1 2)
                           (3 4 5)
                           (6 7 8)
                           (9 10 11))))
    (ensure-same (reshape '(4 t) a) a-reshaped-rm)
    ;; (ensure-same (reshape a '(4 t) :column-major)
    ;;              #2A((0 5 10)
    ;;                  (4 9 3)
    ;;                  (8 2 7)
    ;;                  (1 6 11)))
    ))

;; (addtest (array-tests)
;;   rows-and-columns
;;   (let ((a #2A((1 2)
;;                (3 4)
;;                (5 6)))
;;         (rows (vector #(1 2) #(3 4) #(5 6)))
;;         (columns (vector #(1 3 5) #(2 4 6))))
;;     (ensure-same (rows a) rows)
;;     (ensure-same (columns a) columns)))

;; (addtest (array-tests)
;;   pref
;;   (let ((matrix #2A((0 1)
;;                     (2 3)
;;                     (4 5)))
;;         (vector #(0 1 2 3)))
;;     (ensure-same (pref matrix #(0 2 1) #(1 0 1)) #(1 4 3))
;;     (ensure-same (pref vector #(3 1 2 0)) #(3 1 2 0))
;;     (ensure-error (pref vector #(1) #(0)))
;;     (ensure-error (pref matrix #(1 0) #(0)))))


;; (addtest (array-tests)
;;   filter-rows-test
;;   (let ((matrix (ia 4 3))
;;         (*lift-equality-test* #'equalp)
;;         (expected-result #2A((0 1 2) (6 7 8))))
;;     (ensure-same (filter-rows (lambda (vector) (evenp (aref vector 0))) matrix)
;;                  expected-result)
;;     (ensure-same (with-filter-rows matrix ; so much simpler, eh?
;;                      ((a 0))
;;                    (evenp a))
;;                  expected-result)))

;; (addtest (array-tests)
;;   shrink-rows-test
;;   (ensure-same (shrink-rows (array* '(2 5) t
;;                                     nil 1 2 nil nil
;;                                     nil nil 3 4 nil))
;;                (values (array* '(2 3) t
;;                                1 2 nil
;;                                nil 3 4)
;;                        1 4))
;;   (ensure-same (shrink-rows (make-array '(2 3) :initial-element 'foo)
;;                             :predicate (lambda (x) (not (eq x 'foo))))
;;                nil))

(addtest (array-tests)
  rep
  ;; (ensure-same (rep '(1 2 3) 4 2)
  ;;              '(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3))
  (ensure-same (rep #(1 2 3) 4 2)
               #(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3)))


(addtest (array-tests)
  displace-test
  (let ((a (ia 2 3 4))
        (*lift-equality-test* #'equalp))
    (ensure-same (displace-array a '(2 3 4)) a)
    (ensure-same (displace-array a '(6 4)) (ia 6 4))
    (ensure-same (displace-array a '(10) 14) (ia* 14 10))
    (ensure-same (displace-array a 10 0) (ia 10))
    (ensure-same (subarray a 0 0) (ia 4))
    (ensure-same (subarray a 1) (ia* 12 3 4))))

(addtest (array-tests)
  setf-subarray-tests
  (let ((a (ia 3 4))
        (*lift-equality-test* #'equalp))
    (setf (subarray a 1) #(4 3 2 1))
    (ensure-same a #2A((0 1 2 3)
                       (4 3 2 1)
                       (8 9 10 11)))
    (setf (subarray a 1 3) 9)
    (ensure-same a #2A((0 1 2 3)
                       (4 3 2 9)
                       (8 9 10 11)))))

(addtest (array-tests)
  subarrays-tests
  (let ((a (ia 2 3))
        (b (ia 2 2 3))
        (*lift-equality-test* #'equalp))
    (ensure-same (subarrays 0 a) a)
    (ensure-same (subarrays 1 a) #(#(0 1 2) #(3 4 5)))
    (ensure-same (subarrays 2 a) a)
    (ensure-same (subarrays 0 b) b)
    (ensure-same (subarrays 1 b) #(#2A((0 1 2)
                                       (3 4 5))
                                   #2A((6 7 8)
                                       (9 10 11))))
    (ensure-same (subarrays 2 b) #2A((#(0 1 2) #(3 4 5))
                                     (#(6 7 8) #(9 10 11))))
    (ensure-same (subarrays 3 b) b)
    (ensure-error (subarrays 3 a))
    (ensure-error (subarrays -1 a))
    (let* ((c (make-array '(9 5 7) :element-type 'bit
                                   :initial-element 1))
           (c-sub (subarrays 1 c)))
      (ensure (every (lambda (x)
                       (and (eq (array-element-type x) 'bit)
                            (equal (array-dimensions x) '(5 7))))
                     c-sub)))))

(addtest (array-tests)
  partition-tests
  (let ((*lift-equality-test* #'equalp)
        (a (ia 3 2))
        (b (ia 3)))
    (ensure-same (partition a 0) a)
    (ensure-same (partition a 1) #2A((2 3)
                                     (4 5)))
    (ensure-same (partition a 1 2) #2A((2 3)))
    (ensure-same (partition b 0) b)
    (ensure-same (partition b 1) #(1 2))
    (ensure-same (partition b 2) #(2))))

(addtest (array-tests)
  combine-tests
  (let ((a (ia 4 3 5))
        (*lift-equality-test* #'==))
    (ensure-same (combine (subarrays 0 a)) a)
    (ensure-same (combine (subarrays 1 a)) a)
    (ensure-same (combine (subarrays 2 a)) a)
    (ensure-same (combine (subarrays 3 a)) a)))

(addtest (array-tests)
  valid-permutation-test
  (ensure (valid-permutation? #(0 1 2)))
  (ensure (valid-permutation? #(1 0 2)))
  (ensure (not (valid-permutation? #(0 1 1))))
  (ensure (not (valid-permutation? #(0 1 2) 4)))
  (ensure (not (valid-permutation? #(0 1 2) 2))))

(addtest (array-tests)
  permutation-test
  (let ((a (ia 3 4))
        (b (ia 1 2 3))
        (c (ia 2 2 3))
        (*lift-equality-test* #'equalp))
    (ensure-same (permute a '(0 1)) a)
    (ensure-same (permute a '(1 0)) (transpose a))
    (ensure-same (permute b '(1 2 0))
                 #3A(((0) (1) (2))
                     ((3) (4) (5))))
    (ensure-same (permute c '(2 1 0))
                 #3A(((0 6) (3 9))
                     ((1 7) (4 10))
                     ((2 8) (5 11))))))

(addtest (array-tests)
  which
  (let* ((vector #(7 6 5 4 3 2 1 0))
         (list (coerce vector 'list))
         (even-pos #(1 3 5 7))
         (odd-pos #(0 2 4 6))
         (arbitrary (reverse #(0 2 3 5)))
         (arbitrary-pos #(2 4 5 7))
         (*lift-equality-test* #'equalp))
    (ensure-same (which #'oddp vector) odd-pos)
    (ensure-same (which #'oddp list) odd-pos)
    (ensure-same (which #'evenp vector) even-pos)
    (ensure-same (which #'evenp list) even-pos)
    (flet ((in? (element) (find element arbitrary)))
      (ensure-same (which #'in? vector) arbitrary-pos)
      (ensure-same (which #'in? list) arbitrary-pos))))

(addtest (array-tests)
  mask
  (let* ((vector (iseq 6))
         (odd-bits (mask #'oddp vector))
         (even-bits (mask #'evenp vector))
         (div3-bits (mask (lambda (n) (divides? n 3)) vector))
         (*lift-equality-test* #'equalp))
    (ensure-same even-bits #*101010)
    (ensure-same odd-bits #*010101)
    (ensure-same div3-bits #*100100)
    (ensure-same (sub vector even-bits) #(0 2 4))
    (ensure-same (sub vector odd-bits) #(1 3 5))
    (ensure-same (sub vector div3-bits) #(0 3))
    (ensure-same (sub vector (bit-ior even-bits div3-bits)) #(0 2 3 4))))

(addtest (array-tests)
  bracket-test
  (let ((a #(0 1 2 3 4 3 2 1 0)))
    (ensure-same (bracket #'plusp a) (cons 1 8))
    (ensure-same (bracket (curry #'<= 3) a) (cons 3 6))
    (ensure-same (bracket (curry #'<= 5) a) nil)
    (ensure-same (bracket t #(nil nil nil t t nil nil)) (cons 3 5))))

(addtest (array-tests)
  norm-test
  (let ((a #(1 -2 3))
        (b #(1 #C(0 -4) 3))
        (*lift-equality-test* #'==))
    (ensure-same (norm1 a) 6)
    (ensure-same (norm1 b) 8)
    (ensure-same (norm2 a) (sqrt 14))
    (ensure-same (norm2 b) (sqrt 26))
    (ensure-same (normsup a) 3)
    (ensure-same (normsup b) 4)))

(addtest (array-tests)
  map-columns
  (ensure-same (map-columns #'sum (ia 3 4))
               #(12 15 18 21))
  (ensure-same (map-columns (lambda (col) (vector (sum col) (mean col))) (ia 3 4))
               #2A((12 15 18 21)
                   (4 5 6 7))))

(addtest (array-tests)
  recycle-row-col
  (let ((v (vector* 'fixnum 1 2 3))
        (*lift-equality-test* #'array=))
    (ensure-same (recycle-row v 4)
                 (matrix* 'fixnum v v v v))
    (ensure-same (recycle-column v 4)
                 (matrix* 'fixnum
                          '(1 1 1 1)
                          '(2 2 2 2)
                          '(3 3 3 3)))))
