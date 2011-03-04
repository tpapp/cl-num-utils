;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defstruct (data-frame (:constructor make-data-frame% (matrix column-index)))
  matrix
  column-index)

(defgeneric make-data-frame (matrix column-index &key copy?)
  (:documentation "Create a data frame using MATRIX and COLUMN-INDEX (the latter
  may be used to create an index, if it isn't one).  COPY? forces copying of the
  MATRIX, otherwise may share structure."))

(defmethod make-data-frame (matrix column-index &key copy?)
  (assert (= (array-rank matrix) 2))
  (let ((ncol (array-dimension matrix 1)))
    (assert (and (= (ix-start column-index) 0)
                 (= (ix-end column-index)) ncol))
    (make-data-frame% (maybe-copy-array matrix copy?)
                      column-index)))

(defmethod make-data-frame (matrix (keys sequence) &key copy?)
  (make-data-frame matrix (make-hashed-index keys) :copy? copy?))

(defmethod dims ((data-frame data-frame))
  (array-dimensions (data-frame-matrix data-frame)))

(defmethod element-type ((data-frame data-frame))
  (array-element-type (data-frame-matrix data-frame)))

(defun data-frame-resolve-index-specification (data-frame index-specification)
  (bind (((:structure data-frame- column-index) data-frame)
         ((:flet resolve (key))
          (typecase key
            ((eql t) key)
            ((or symbol list) (ix column-index key))
            (otherwise key))))
    (if (vectorp index-specification)
        (map 'vector #'resolve index-specification)
        (resolve index-specification))))

(defmacro data-frame-with-resolved-index-specification
    ((((matrix column-index) data-frame)
      ((is0 is1) index-specifications))
     &body body)
  ""
  (check-type matrix symbol)
  (check-type column-index symbol)
  (check-type is0 symbol)
  (check-type is1 symbol)
  (once-only (data-frame)
    `(bind (((:structure data-frame- (,matrix matrix)
                         (,column-index column-index)) data-frame)
            ((,is0 ,is1) ,index-specifications)
            (,is1 (data-frame-resolve-index-specification ,data-frame ,is1)))
       (declare (ignorable ,matrix ,column-index ,is0 ,is1))
       ,@body)))

(defmethod sub ((data-frame data-frame) &rest index-specifications)
  (data-frame-with-resolved-index-specification 
      (((matrix column-index) data-frame)
       ((is0 is1) index-specifications))
    (let ((sub-matrix (sub matrix is0 is1)))
      (if (vectorp sub-matrix)
          sub-matrix
          (make-data-frame% sub-matrix (sub column-index is1))))))

(defmethod (setf sub) ((array array) (data-frame data-frame)
                       &rest index-specifications)
  (data-frame-with-resolved-index-specification
      (((matrix column-index) data-frame)
       ((is0 is1) index-specifications))
    (setf (sub matrix is0 is1) array)))

(defmethod filter-rows (predicate (data-frame data-frame))
  (bind (((:structure data-frame- matrix column-index) data-frame))
    (make-data-frame% (filter-rows predicate matrix) column-index)))

(defmacro with-filter-data-frame (data-frame (&rest name-key-pairs) &body body)
  "Similar to WITH-FILTER-ROWS, but also allows keys.  If the latter is not
given, the quoted name will be used instead."
  (with-unique-names (resolve index)
    (let ((name-column-pairs
           (mapcar (lambda (name-key-pair)
                     (bind (((name &optional (key `',name))
                             (alexandria:ensure-list name-key-pair)))
                       `(,name (,resolve ,key))))
                   name-key-pairs)))
      (once-only (data-frame)
        `(let ((,index (data-frame-column-index ,data-frame)))
           (flet ((,resolve (key)
                    (if (typep key 'fixnum)
                        key
                        (ix ,index key))))
             (with-filter-rows ,data-frame ,name-column-pairs
               ,@body)))))))

(defmethod map-columns (function (data-frame data-frame))
  (bind (((:structure data-frame- matrix column-index) data-frame)
         (result (map-columns function matrix)))
    (if (vectorp result)
        result
        (make-data-frame% result column-index))))

(defmethod as-array ((data-frame data-frame) &key copy?)
  (maybe-copy-array (data-frame-matrix data-frame) copy?))

;;; !! maybe write compiler macro for
;;; !! (setf (sub data-frame ..) (matrix data-frame ...))
