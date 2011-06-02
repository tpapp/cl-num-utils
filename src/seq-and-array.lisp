;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)




;;; !! define compiler macros for VECTOR* and ARRAY*

;; (declaim (inline pick))
;; (defun pick (&rest indexes)
;;   "Return a closure that returns array elements with given indexes."
;;   (lambda (value) (apply #'aref value indexes)))




;; (defun map-array (function array &optional (element-type t))
;;   "Map array elementwise."
;;   (aprog1 (make-array (array-dimensions array) :element-type element-type)
;;     (dotimes (index (array-total-size array))
;;       (setf (row-major-aref it index)
;;             (funcall function (row-major-aref array index))))))

;; (defun vector-satisfies? (vector predicate)
;;   "Return non-nil iff vector satisfies predicate elementwise.
;; Example: (vector-elementwise? vector #'<) tests if vector is strictly
;; increasing."
;;   (check-type vector vector)
;;   (when (< (length vector) 2)
;;     (return-from vector-satisfies? t))
;;   (iter
;;     (for element :in-vector vector :from 1)
;;     (for element-p :previous element :initially (aref vector 0))
;;     (always (funcall predicate element-p element))))


;; (defun sort-order (sequence predicate &key key)
;;   "Sort elements of SEQUENCE using PREDICATE (and optionally, KEY).  Return
;; the permutation of elements as the second value (which is always
;; a (SIMPLE-ARRAY FIXNUM (*)).  Functional and nondestructive."
;;   (let* ((index 0)
;;          (paired (map 'simple-vector
;;                       (lambda (element)
;;                         (prog1 (cons element index)
;;                           (incf index)))
;;                       sequence))
;;          (paired (sort paired predicate
;;                        :key (if key (compose key #'car) #'car))))
;;     (values (map (type-of sequence) #'car paired)
;;             (map '(simple-array fixnum (*)) #'cdr paired))))



;; (defgeneric rep (sequence times &optional each)
;;   (:documentation "Return a new sequence, which contains SEQUENCE repeated TIMES
;; times, repeating each element EACH times (default is 1)."))

;; (defmethod rep ((list list) times &optional (each 1))
;;   (iter :outer
;;     (repeat times)
;;     (iter
;;       (for elt :in list)
;;       (iter
;;         (repeat each)
;;         (in :outer 
;;             (collecting elt))))))



;; (defun group (sequence &rest indexes)
;;   "Return an array, which contains sequences of the same type as SEQUENCE,
;; with elements grouped according to the indexes (which are expected to be
;; nonnegative fixnums).  The maximum index for each dimension is calculated
;; automatically, and they detemine the dimensions of the result.  Order of the
;; elements is preserved."
;;   (unless indexes
;;     (return-from group sequence))
;;   (let* ((indexes (mapcar (lambda (index) (coerce index 'vector)) indexes))
;;          (dimensions (mapcar (lambda (index) (1+ (reduce #'max index))) indexes))
;;          (result (make-array dimensions :initial-element nil))
;;          (length (length sequence)))
;;     (assert (every (lambda (index) (= (length index) length)) indexes) ()
;;             "Indexes must be the same length as the sequence.")
;;     (map nil
;;          (let ((i 0))
;;            (lambda (element)
;;              (push element
;;                    (apply #'aref result 
;;                           (mapcar (lambda (index) (aref index i)) indexes)))
;;              (incf i)))
;;          sequence)
;;     (let ((sequence-type
;;            (etypecase sequence
;;              (list 'list)
;;              (vector `(simple-array ,(array-element-type sequence) (*))))))
;;       (dotimes (i (array-total-size result))
;;         (setf (row-major-aref result i)
;;               (nreverse (coerce (row-major-aref result i) sequence-type)))))
;;     result))


(defgeneric which-rows (predicate object)
  (:documentation "Return a simple-bit-vector, flagging the rows of a matrix 
using predicate."))

(defmethod which-rows (predicate (matrix array))
  (let+ (((n-row nil) (array-dimensions matrix))
         (flag (predicate-as-flag predicate))
         (result (make-array n-row :element-type 'bit)))
    (dotimes (row-index n-row)
      (setf (aref result row-index)
            (funcall flag (subarray matrix row-index))))
    result))

;;; !!! compiler macros for (support (which...)), or maybe even name them

