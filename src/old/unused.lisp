;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file contains functions which are not used at the moment,
  should not be loaded."))

;; (defgeneric filter-rows (predicate object)
;;   (:documentation "Filter rows of a matrix, with predicate applied to each row
;; as vectors (which should not be modified).")
;;   (:method (predicate (object array))
;;     (sub object (which-rows predicate object) t)))

;; (defmacro with-filter-rows (matrix (&rest name-column-pairs) &body body)
;;   "Use BODY to filter rows of MATRIX, binding NAMEs to the given COLUMNs.

;; Example:
;;  (with-filter-rows #2A((0 1)
;;                        (101 80)
;;                        (203 200))
;;      ((a 0)
;;       (b 1))
;;      (and (oddp a) (< 100 b)))    ; => #2A((203 200))"
;;   (with-unique-names (vector)
;;     (let ((name-var-values (mapcar (lambda (name-column-pair)
;;                                      (let+ (((name column) name-column-pair))
;;                                        (check-type name symbol)
;;                                        (list name
;;                                              (gensym (symbol-name name))
;;                                              column)))
;;                                    name-column-pairs)))
;;       `(let ,(mapcar #'cdr name-var-values)
;;          (filter-rows (lambda (,vector)
;;                         (let ,(mapcar (lambda (name-var-value)
;;                                         (let+ (((name var nil) name-var-value))
;;                                           `(,name (aref ,vector ,var))))
;;                                name-var-values)
;;                           ,@body))
;;                       ,matrix)))))

;; (defgeneric shrink-rows (matrix &key predicate)
;;   (:documentation "Drop columns where no element satisfies predicate from both sides
;;   of MATRIX.  The default predicate is the identity function, ie columns of all NILs
;;   are dropped.  If no element satisfies PREDICATE, NIL is returned, otherwise the
;;   shrunk array, the start index and the end index are returned as values.")
;;   (:method ((matrix array) &key (predicate #'identity))
;;     (let+ (((nrow nil) (array-dimensions matrix)))
;;       (iterate
;;         (for row-index :below nrow)
;;         (let* ((row (subarray matrix row-index))
;;                (row-left (position-if predicate row)))
;;           (when row-left
;;             (let ((row-right (position-if predicate row :from-end t)))
;;               (minimize row-left :into left)
;;               (maximize row-right :into right))))
;;         (finally
;;          (return
;;            (when (and left right)
;;              (let ((end (1+ right)))
;;                (values (sub matrix t (si left end)) left end)))))))))

;;; !! ROWS and COLUMNS could be speeded up considerably for Lisp arrays

;; (defgeneric rows (object &key copy?)
;;   (:documentation "Return the rows of a matrix-like OBJECT as a vector.  May
;;   share structure unless COPY?.")
;;   (:method ((matrix array) &key copy?)
;;     (iter
;;       (for row-index :below (nrow matrix))
;;       (collecting (subarray matrix row-index :copy? copy?)
;;                   :result-type vector)))
;;   (:method (object &key copy?)
;;     (rows (as-array object) :copy? copy?)))

;; (defgeneric columns (matrix &key copy?)
;;   (:documentation "Return the columns of a matrix-like object as a vector of
;;   vectors.  May share structure unless COPY?.")
;;   (:method ((matrix array) &key copy?)
;;     (declare (ignore copy?))
;;     (iter
;;       (for column-index :below (ncol matrix))
;;       (collecting (sub matrix t column-index)
;;                   :result-type vector)))
;;   (:method (object &key copy?)
;;     (columns (as-array object) :copy? copy?)))


;; (defgeneric map-rows (function matrix)
;;   (:documentation "Map matrix row-wise into another matrix or vector, depending
;;  on the element type returned by FUNCTION."))

;; (defun map-subarrays (function array)
;;   "Map subarrays along the first index, constructing a result array with .
;; Single-element subarrays are treated as atoms."
;;   (let+ (((&values length get-subarray)
;;           (if (vectorp array)
;;               (values (length array)
;;                       (lambda (index) (aref array index)))
;;               (values (nrow array)
;;                       (lambda (index) (subarray array index)))))
;;          results
;;          save-subarray)
;;     (dotimes (index length)
;;       (let ((result (funcall function (funcall get-subarray))))
;;         (when (zerop index)
;;           (setf (values results save-subarray)
;;                 (if (arrayp result)
;;                     (values
;;                      (make-array (cons length (array-dimensions result))
;;                                  :element-type (array-element-type result))
;;                      (lambda (index result)
;;                        (setf (subarray results index) result)))
;;                     (values (make-array length)
;;                             (lambda (index result)
;;                               (setf (aref results index) result))))))
;;         (funcall save-subarray index result)))))


;; (defgeneric create (type element-type &rest dimensions)
;;   (:documentation "Create an object of TYPE with given DIMENSIONS and
;;   ELEMENT-TYPE (or a supertype thereof)."))

;; (defmethod create ((type (eql 'array)) element-type &rest dimensions)
;;   (make-array dimensions :element-type element-type))

;; (defmethod collect-rows (nrow function &optional (type 'array))
;;   (let (result  ncol)
;;     (iter
;;       (for row :from 0 :below nrow)
;;       (let ((result-row (funcall function)))
;;         (when (first-iteration-p)
;;           (setf ncol (length result-row)
;;                 result (create type (array-element-type result-row) nrow ncol)))
;;         (setf (sub result row t) result-row)))
;;     result))

;; (defun collect-vector (n function &optional (element-type t))
;;   (let (result)
;;     (iter
;;       (for index :from 0 :below n)
;;       (let ((element (funcall function)))
;;         (when (first-iteration-p)
;;           (setf result (make-array n :element-type element-type)))
;;         (setf (aref result index) element)))
;;     result))

(defgeneric pref (object &rest indexes)
  (:documentation "Return a vector, with elements from OBJECT, extracted using
  INDEXES in parallel."))

(defmethod pref ((array array) &rest indexes)
  (let ((rank (array-rank array))
        (element-type (array-element-type array)))
    (assert (= rank (length indexes)))
    (when (zerop rank)
      (return-from pref (make-array 0 :element-type element-type)))
    (let* ((length (length (first indexes)))
           (result (make-array length :element-type element-type)))
      (assert (every (lambda (index) (= (length index) length)) (cdr indexes)))
      (loop
        :for element-index :below length
        :do (setf (aref result element-index)
                  (apply #'aref array
                                (mapcar (lambda (index) (aref index element-index))
                                        indexes))))
      result)))

;; (defun sequence= (a b)
;;   "Test equality of A and B elementwise (also tests that elements are
;; of the same type)."
;;   (and (if (and (vectorp a) (vectorp b))
;;            (equal (array-element-type a)
;;                   (array-element-type b))
;;            (and (listp a) (listp b)))
;;        (every #'eql a b)))

;; (addtest (seq-and-array-tests)
;;   vector*-and-array*
;;   (let+ ((*lift-equality-test*
;;           (lambda (array spec)
;;             "Test that array conforms to spec, which is (element-type array)."
;;             (and (type= (array-element-type array)
;;                         (upgraded-array-element-type (first spec)))
;;                  (equalp array (second spec))))))
;;     (ensure-same (vector* 'fixnum 3 5 7) '(fixnum #(3 5 7)))
;;     (ensure-same (array* '(2 3) 'double-float
;;                          3 5 7
;;                          11 13 17)
;;                  '(double-float #2A((3d0 5d0 7d0) (11d0 13d0 17d0))))))

;; (addtest (seq-and-array-tests)
;;   sequence=
;;   (ensure-same (vector* 'double-float 1 2 3)
;;                (vector* 'double-float 1 2.0 3d0))
;;   (ensure-same '(1 2 3) '(1 2 3))
;;   (ensure-different '(1d0 2 3) '(1 2 3))
;;   (ensure-different '(1 2 3) (vector* 'fixnum 1 2 3)))

;; (addtest (seq-and-array-tests)
;;   seq
;;   ;; missing :LENGTH (default :BY)
;;   (ensure-same (numseq 0 5)
;;                (vector* 'fixnum 0 1 2 3 4 5))
;;   ;; missing :TO
;;   (ensure-same (numseq 1 nil :by 1/2 :length 3 :type 'list)
;;                '(1 3/2 2))
;;   ;; missing :FROM
;;   (ensure-same (numseq nil 9 :by 1d0 :length 4)
;;                (vector* 'double-float 6d0 7d0 8d0 9d0))
;;   ;; missing :LENGTH, automatic direction for :by
;;   (ensure-same (numseq 9 8 :by 0.5 :type 'list)
;;                '(9.0 8.5 8.0))
;;   (ensure-same (numseq 9 8 :by -0.5 :type 'list)
;;                '(9.0 8.5 8.0)))

;; (addtest (seq-and-array-tests)
;;   map-array
;;   (let ((a (map-array #'1+ (ia 3 4) 'fixnum))
;;         (*lift-equality-test* #'equalp))
;;     (ensure-same a (ia* 1 3 4))
;;     (ensure-same (array-element-type a) 'fixnum)))

;; (addtest (seq-and-array-tests)
;;   vector-satisfies?
;;   (ensure (vector-satisfies? #(1 2 3) #'<))
;;   (ensure (not (vector-satisfies? #(1 1 2) #'<)))
;;   (ensure (not (vector-satisfies? #(3 2 1) #'<=)))
;;   (ensure (vector-satisfies? #(1) #'<))
;;   (ensure (vector-satisfies? #() #'<))
;;   (ensure-error (vector-satisfies? 'not-a-vector #'<))
;;   (ensure-error (vector-satisfies? '(not a vector) #'<)))


;; (addtest (seq-and-array-tests)
;;   group-test
;;   (let ((*lift-equality-test* #'equalp)
;;         (v6 #(0 1 2 3 4 5)))
;;     (ensure-same (group v6 #(0 1 2 0 1 0))
;;                  #(#(0 3 5) #(1 4) #(2)))
;;     (ensure-same (group v6 #(0 1 2 0 1 0) #(0 1 0 0 1 1))
;;                  #2A((#(0 3) #(5))
;;                      (#() #(1 4))
;;                      (#(2) #())))
;;     (ensure-error (group v6 #(1 2 3)))
;;     (ensure-error (group v6 #(1 2 3 4 5 6) nil))))

;; (addtest (array-tests)
;;   map-rows
;;   (ensure-same (map-rows #'sum (ia 4 3))
;;                #(3 12 21 30))
;;   (ensure-same (map-rows (lambda (col) (vector (sum col) (mean col))) (ia 4 3))
;;                #2A((3 1)
;;                    (12 4)
;;                    (21 7)
;;                    (30 10))))
