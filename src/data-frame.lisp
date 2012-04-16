;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; QUESTION: equal is hardwired, is that general enough?
;;; QUESTION: should positive integers work as keys de facto, resolving to themselves?
;;; QUESTION: should data frames with different column orders but same content be ==?

(defclass data-frame ()
  ((keys :reader keys :initform #() :initarg :keys)
   (columns :initform #() :initarg :columns)
   (table :initform (make-hash-table :test #'equal) :initarg :table))
  (:documentation "An ordered collection of vectors, which are also accessible
with keys (compared with EQUAL).

Implementation notes:

  - a hash table is used to speed up key lookup, but strictly speaking is not
    part of the interface."))

(defmethod print-object ((data-frame data-frame) stream)
  (let+ (((&slots-r/o keys columns) data-frame))
    (print-unreadable-object (data-frame stream :type t)
      (format stream "keys:")
      (loop for key across keys do (format stream " ~S" key))
      (loop for key across keys
            for column across columns
            do (format stream "~&~2T~A: ~A" key column)))))

(defmethod nrow ((data-frame data-frame))
  (length (first* (slot-value data-frame 'columns))))

(defmethod ncol ((data-frame data-frame))
  (length (slot-value data-frame 'columns)))

(defmethod as-array ((data-frame data-frame) &key)
  (columns-to-matrix (slot-value data-frame 'columns)))

(defun copy-data-frame (data-frame &optional (map-columns #'identity))
  "Make a copy of DATA-FRAME.  MAP-COLUMNS is used to copy columns."
  (let+ (((&slots-r/o keys columns table) data-frame))
    (make-instance 'data-frame
                   :keys keys
                   :columns (map 'vector map-columns columns)
                   :table table)))

(defun add-columns2 (data-frame new-keys new-columns)
  "Add columns to a data frame, which is modified (and also returned)."
  (assert (length= new-keys new-columns))
  (let+ (((&slots keys columns table) data-frame)
         (index (length keys))
         (new-table (copy-hash-table table
                                     :size (+ index (length new-keys))))
         (length (common new-columns :key #'length)))
    (assert (and length
                 (if (plusp index)
                     (length= (first* columns) length)
                     t))
            () "Column length mismatch.")
    (map nil (lambda (key)
               (let+ (((&values nil present?) (gethash key table)))
                 (assert (not present?) () "Duplicate key ~A." key)
                 (setf (gethash key new-table) index)
                 (incf index)))
         new-keys)
    (setf keys (concatenate 'vector keys new-keys)
          columns (concatenate 'vector columns new-columns)
          table new-table))
  data-frame)

(defun add-columns (data-frame key-column-alist)
  "Add columns to a data frame, which is modified (and also returned)."
  (add-columns2 data-frame (map 'vector #'car key-column-alist)
                (map 'vector #'cdr key-column-alist)))

(defun add-column (data-frame key column)
  "Add a column to the data frame."
  (add-columns2 data-frame (list key) (list column)))

(defmethod == ((df1 data-frame) (df2 data-frame)
               &optional (tolerance *==-tolerance*))
  (let+ (((&slots-r/o (v1 columns) (k1 keys)) df1)
         ((&slots-r/o (v2 columns) (k2 keys)) df2))
    (and (every (==* tolerance) v1 v2)
         (every #'equal k1 k2))))

(defun make-data-frame (key-column-alist)
  "Make a data-frame."
  (add-columns (make-instance 'data-frame) key-column-alist))

(defun make-data-frame2 (keys columns)
  "Make a data-frame."
  (add-columns2 (make-instance 'data-frame) keys columns))

(defun matrix-to-data-frame (matrix keys)
  "Convert a matrix to a data frame with the given keys."
  (make-data-frame2 (coerce keys 'vector) (matrix-to-columns matrix)))

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
         ((&slots-r/o columns keys) data-frame)
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
      (t (make-data-frame2
          (sub keys col-selection)
          (map1 (lambda (col) (sub col row-selection))
                (sub columns col-selection)))))))

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

(defun map-into-data-frame (data-frame keys function key
                            &optional (element-type t))
  "Use map-data-frame to create a column with KEY."
  (add-columns2 data-frame
                (vector key)
                (vector (map-data-frame data-frame keys function
                                        `(simple-array ,element-type (*))))))
