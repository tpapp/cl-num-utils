;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defstruct (data-frame (:constructor make-data-frame% (matrix column-index)))
  matrix
  column-index)

(defmethod make-data-frame (matrix column-index)
  (assert (= (array-rank matrix) 2))
  (let ((ncol (array-dimension matrix 1)))
    (assert (and (= (ix-start column-index) 0)
                 (= (ix-end column-index)) ncol))
    (make-data-frame% matrix column-index)))

(defmethod make-data-frame (matrix (keys sequence))
  (make-data-frame matrix (make-hashed-index keys)))

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

(defmethod sub ((data-frame data-frame) &rest index-specifications)
  (bind (((is0 is1) index-specifications))
    (sub (data-frame-matrix data-frame) is0
         (data-frame-resolve-index-specification data-frame is1))))

;;; !! write setf methods
