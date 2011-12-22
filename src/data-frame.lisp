;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass data-frame ()
  ((keys :initarg :keys :reader keys)
   (columns :initarg :columns)
   (table :initarg :table)))

(defmethod print-object ((data-frame data-frame) stream)
  (let+ (((&slots-r/o keys columns) data-frame))
    (print-unreadable-object (data-frame stream :type t)
      (loop for key across keys
            for column across columns
            do (format stream "~&~2T~A: ~A" key column)))))

(defmethod nrow ((data-frame data-frame))
  (length (first* (slot-value data-frame 'columns))))

(defmethod ncol ((data-frame data-frame))
  (length (slot-value data-frame 'columns)))

(defun data-frame-test (data-frame)
  "Return the function used for comparing keys."
  (hash-table-test (slot-value data-frame 'table)))

(defmethod as-array ((data-frame data-frame) &key)
  (columns-to-matrix (slot-value data-frame 'columns)))

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
  (let+ (((&slots-r/o (v1 columns) (k1 keys)) df1)
         ((&slots-r/o (v2 columns) (k2 keys)) df2)
         ((&accessors-r/o (t1 data-frame-test)) df1)
         ((&accessors-r/o (t2 data-frame-test)) df2))
    (and (every (==* tolerance) v1 v2)
         (eq t1 t2)
         (every t1 k1 k2))))

(defun make-data-frame% (keys columns test)
  (declare (optimize debug))
  (assert (length= keys columns))
  (assert (common columns :key #'length))
  (check-types (keys columns) vector)
  (make-instance 'data-frame
                 :columns columns
                 :keys keys
                 :table (data-frame-keys-table keys test)))

(defun make-data-frame (key-column-alist &key (test #'equal))
  "Make a data-frame."
  (make-data-frame% (map 'vector #'car key-column-alist)
                    (map 'vector #'cdr key-column-alist)
                    test))

(defun matrix-to-data-frame (matrix keys &key (test #'equal))
  "Convert a matrix to a data frame with the given keys."
  (make-data-frame% (coerce keys 'vector) (matrix-to-columns matrix) test))

(defun data-frame-resolve-keys (data-frame keys)
  "Resolve data frame keys, returning  a format that can be passed to SUB."
  (let+ (((&slots-r/o columns table) data-frame)
         ((&flet resolve% (key)
            (let+ (((&values index present?) (gethash key table)))
              (assert present? () "Key ~A not found." key)
              index))))
    (sub-resolve-selection (etypecase keys
                             (string (resolve% keys))
                             (vector (map 'vector #'resolve% keys))
                             ((eql t) t)
                             (t (resolve% keys)))
                           (length columns) t)))

(defmethod sub ((data-frame data-frame) &rest selections)
  (let+ (((row-selection col-selection) selections)
         ((&slots-r/o columns keys table) data-frame)
         (col-selection (data-frame-resolve-keys data-frame col-selection))
         (row-selection (sub-resolve-selection row-selection (nrow data-frame))))
    (cond
      ((and (fixnum? col-selection) (fixnum? row-selection))
       (aref (aref columns col-selection) row-selection))
      ((fixnum? col-selection)         ; result is a vector (column)
       (sub (aref columns col-selection)
            row-selection))
      ((fixnum? row-selection)          ; result is a vector (row)
       (map1 (lambda (col) (aref col row-selection))
             (sub columns col-selection)))
      (t (make-data-frame%
          (sub keys col-selection)
          (map1 (lambda (col) (sub col row-selection))
                (sub columns col-selection))
          (hash-table-test table))))))

(defmethod (setf sub) (value (data-frame data-frame) &rest selections)
  (let+ (((row-selection col-selection) selections)
         ((&slots-r/o columns) data-frame)
         (col-selection (data-frame-resolve-keys data-frame col-selection))
         )
    (if (fixnum? col-selection)
        (setf (sub (aref columns col-selection) row-selection) value)
        (map nil (lambda (col)
                   (setf (sub (aref columns col) row-selection)
                         (sub value row-selection col)))
             col-selection))
    value))

(defun map-data-frame (data-frame keys function
                       &optional (result-type 'vector))
  "Map columns of a data frame that correspond to keys using FUNCTION,
returning a sequence of the given RESULT-TYPE."
  (let+ (((&slots-r/o columns table) data-frame))
    (apply #'map result-type function
           (map 'list
                (lambda (key) (aref columns (gethash* key table)))
                keys))))

(defun extend-data-frame (data-frame key-column-alist)
  "Add columns to a data frame."
  (let+ (((&slots-r/o keys columns) data-frame))
    (make-data-frame% (concatenate 'vector keys 
                                   (map 'vector #'car key-column-alist))
                      (concatenate 'vector columns
                                   (map 'vector #'cdr key-column-alist))
                      (data-frame-test data-frame))))

(defun map-extend-data-frame (data-frame keys function key
                            &optional (element-type t))
  "Add the mapped column to the data frame with KEY."
  (extend-data-frame
   data-frame 
   (list 
    (cons key 
          (map-data-frame data-frame keys function
                          `(simple-array ,element-type (*)))))))
