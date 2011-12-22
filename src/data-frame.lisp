;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass data-frame ()
  ((keys :initarg :keys :reader keys)
   (vectors :initarg :vectors)
   (table :initarg :table)))

(defmethod nrow ((data-frame data-frame))
  (length (first* (slot-value data-frame 'vectors))))

(defmethod ncol ((data-frame data-frame))
  (length (slot-value data-frame 'vectors)))

(defun data-frame-test (data-frame)
  "Return the function used for comparing keys."
  (hash-table-test (slot-value data-frame 'table)))

(defmethod as-array ((data-frame data-frame) &key)
  (columns-to-matrix (slot-value data-frame 'vectors)))

(defun data-frame-keys-table (keys test)
  "Generate a hash table for looking up keys in data frame, also check that
there are no duplicate keys."
  (let ((table (make-hash-table :test test :size (length keys)))
        (index 0))
    (map nil (lambda (key)
               (let+ (((&values nil present?) (gethash key table)))
                 (assert (not present?) () "Duplicate key ~A." key)
                 (setf (gethash key table) index)
                 (incf index)))
         keys)
    table))

(defmethod == ((df1 data-frame) (df2 data-frame)
               &optional (tolerance *==-tolerance*))
  (let+ (((&slots-r/o (v1 vectors) (k1 keys)) df1)
         ((&slots-r/o (v2 vectors) (k2 keys)) df2)
         ((&accessors-r/o (t1 data-frame-test)) df1)
         ((&accessors-r/o (t2 data-frame-test)) df2))
    (and (every (==* tolerance) v1 v2)
         (eq t1 t2)
         (every t1 k1 k2))))

(defun make-data-frame% (keys vectors test)
  (declare (optimize debug))
  (assert (length= keys vectors))
  (assert (common vectors :key #'length))
  (check-types (keys vectors) vector)
  (make-instance 'data-frame
                 :vectors vectors
                 :keys keys
                 :table (data-frame-keys-table keys test)))

(defun make-data-frame (key-vector-alist &key (test #'equal))
  "Make a data-frame."
  (make-data-frame% (map 'vector #'car key-vector-alist)
                    (map 'vector #'cdr key-vector-alist)
                    test))

(defun matrix-to-data-frame (matrix keys &key (test #'equal))
  "Convert a matrix to a data frame with the given keys."
  (make-data-frame% (coerce keys 'vector) (matrix-to-columns matrix) test))

(defun data-frame-resolve-keys (data-frame keys)
  "Resolve data frame keys, returning  a format that can be passed to SUB."
  (let+ (((&slots-r/o vectors table) data-frame)
         ((&flet resolve% (key)
            (let+ (((&values index present?) (gethash key table)))
              (assert present? () "Key ~A not found." key)
              index))))
    (sub-resolve-selection (etypecase keys
                             (string (resolve% keys))
                             (vector (map 'vector #'resolve% keys))
                             ((eql t) t)
                             (t (resolve% keys)))
                           (length vectors) t)))

(defmethod sub ((data-frame data-frame) &rest selections)
  (let+ (((row-selection col-selection) selections)
         ((&slots-r/o vectors keys table) data-frame)
         (col-selection (data-frame-resolve-keys data-frame col-selection))
         (row-selection (sub-resolve-selection row-selection (nrow data-frame))))
    (cond
      ((and (fixnum? col-selection) (fixnum? row-selection))
       (aref (aref vectors col-selection) row-selection))
      ((fixnum? col-selection)         ; result is a vector (column)
       (sub (aref vectors col-selection)
            row-selection))
      ((fixnum? row-selection)          ; result is a vector (row)
       (map1 (lambda (col) (aref col row-selection))
             (sub vectors col-selection)))
      (t (make-data-frame%
          (sub keys col-selection)
          (map1 (lambda (col) (sub col row-selection))
                (sub vectors col-selection))
          (hash-table-test table))))))

(defmethod (setf sub) (value (data-frame data-frame) &rest selections)
  (let+ (((row-selection col-selection) selections)
         ((&slots-r/o vectors) data-frame)
         (col-selection (data-frame-resolve-keys data-frame col-selection))
         )
    (if (fixnum? col-selection)
        (setf (sub (aref vectors col-selection) row-selection) value)
        (map nil (lambda (col)
                   (setf (sub (aref vectors col) row-selection)
                         (sub value row-selection col)))
             col-selection))
    value))
