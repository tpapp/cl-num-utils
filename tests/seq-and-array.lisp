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

(deftestsuite seq-and-array-tests (cl-num-utils-tests)
  ()
  (:equality-test #'sequence=))

(addtest (seq-and-array-tests)
  vector*-and-array*
  (bind ((*lift-equality-test* 
          (lambda (array spec)
            "Test that array conforms to spec, which is (element-type array)."
            (and (type= (array-element-type array)
                        (upgraded-array-element-type (first spec)))
                 (equalp array (second spec))))))
    (ensure-same (vector* 'fixnum 3 5 7) '(fixnum #(3 5 7)))
    (ensure-same (array* '(2 3) 'double-float
                         3 5 7
                         11 13 17)
                 '(double-float #2A((3d0 5d0 7d0) (11d0 13d0 17d0))))))

(addtest (seq-and-array-tests)
  sequence=
  (ensure-same (vector* 'double-float 1 2 3)
               (vector* 'double-float 1 2.0 3d0))
  (ensure-same '(1 2 3) '(1 2 3))
  (ensure-different '(1d0 2 3) '(1 2 3))
  (ensure-different '(1 2 3) (vector* 'fixnum 1 2 3)))

(addtest (seq-and-array-tests)
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

(addtest (seq-and-array-tests)
  map-array
  (let ((a (map-array #'1+ (ia 3 4) 'fixnum))
        (*lift-equality-test* #'equalp))
    (ensure-same a (ia* 1 3 4))
    (ensure-same (array-element-type a) 'fixnum)))

(addtest (seq-and-array-tests)
  vector-satisfies?
  (ensure (vector-satisfies? #(1 2 3) #'<))
  (ensure (not (vector-satisfies? #(1 1 2) #'<)))
  (ensure (not (vector-satisfies? #(3 2 1) #'<=)))
  (ensure (vector-satisfies? #(1) #'<))
  (ensure (vector-satisfies? #() #'<))
  (ensure-error (vector-satisfies? 'not-a-vector #'<))
  (ensure-error (vector-satisfies? '(not a vector) #'<)))

(addtest (seq-and-array-tests)
  rep
  (ensure-same (rep '(1 2 3) 4 2)
               '(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3))
  (ensure-same (rep #(1 2 3) 4 2)
               #(1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3)))

(addtest (seq-and-array-tests)
  concat-test
  (ensure-same (concat #(1 2 3) #(4 5 6) (list  7) '(8 9 10))
               (numseq 1 10 :type t) :test #'equalp))

(addtest (seq-and-array-tests)
  displace-test
  (let ((a (ia 2 3 4))
        (*lift-equality-test* #'equalp))
    (ensure-same (displace-array a '(2 3 4)) a)
    (ensure-same (displace-array a '(6 4)) (ia 6 4))
    (ensure-same (displace-array a '(10) 14) (ia* 14 10))
    (ensure-same (displace-array a 10 0) (ia 10))
    (ensure-same (subarray a '(0 0)) (ia 4))
    (ensure-same (subarray a 1) (ia* 12 3 4))))

(addtest (seq-and-array-tests)
  setf-subarray-tests
  (let ((a (ia 3 4)))
    (setf (subarray a 1) #(4 3 2 1))
    (ensure-same a #2A((0 1 2 3)
                       (4 3 2 1)
                       (8 9 10 11))
                 :test #'equalp)))

(addtest (seq-and-array-tests)
  group-test
  (let ((*lift-equality-test* #'equalp)
        (v6 #(0 1 2 3 4 5)))
    (ensure-same (group v6 #(0 1 2 0 1 0))
                 #(#(0 3 5) #(1 4) #(2)))
    (ensure-same (group v6 #(0 1 2 0 1 0) #(0 1 0 0 1 1))
                 #2A((#(0 3) #(5))
                     (#() #(1 4))
                     (#(2) #())))
    (ensure-error (group v6 #(1 2 3)))
    (ensure-error (group v6 #(1 2 3 4 5 6) nil))))

(addtest (seq-and-array-tests)
  which-positions
  (let* ((vector #(7 6 5 4 3 2 1 0))
         (list (coerce vector 'list))
         (even-pos #(1 3 5 7))
         (odd-pos #(0 2 4 6))
         (arbitrary (reverse #(0 2 3 5)))
         (arbitrary-pos #(2 4 5 7))
         (*lift-equality-test* #'equalp))
    (ensure-same (which-positions #'oddp vector) odd-pos)
    (ensure-same (which-positions #'oddp list) odd-pos)
    (ensure-same (which-positions #'evenp vector) even-pos)
    (ensure-same (which-positions #'evenp list) even-pos)
    (flet ((in? (element) (find element arbitrary)))
      (ensure-same (which-positions #'in? vector) arbitrary-pos)
      (ensure-same (which-positions #'in? list) arbitrary-pos))))

(addtest (seq-and-array-tests)
  which
  (let* ((vector (iseq 6))
         (odd-bits (which #'oddp vector))
         (even-bits (which #'evenp vector))
         (div3-bits (which (lambda (n) (divides? n 3)) vector))
         (*lift-equality-test* #'equalp))
    (ensure-same even-bits #*101010)
    (ensure-same odd-bits #*010101)
    (ensure-same div3-bits #*100100)
    (ensure-same (sub vector even-bits) #(0 2 4))
    (ensure-same (sub vector odd-bits) #(1 3 5))
    (ensure-same (sub vector div3-bits) #(0 3))
    (ensure-same (sub vector (bit-ior even-bits div3-bits)) #(0 2 3 4))))
