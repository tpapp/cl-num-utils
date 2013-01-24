;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun interaction (&rest binned-datas)
  "Interaction of binned data series.  Return discrete-binned-data, where the
bins refer to subscripts in row-major ordering (see keys)."
  (declare (optimize debug))
  (let* ((dimensions (mapcar #'bin-limit binned-datas))
         (bins (mapcar #'indexes binned-datas))
         (length (length (first bins)))
         (which (make-array dimensions :element-type 'bit :initial-element 0))
         (table (make-hash-table :test #'eql)))
    (assert (every (lambda (b) (= length (length b))) (cdr bins))
            () "Indexes don't have the same length.")
    (let* (;; flag and save row-major positions
           (row-major-positions
            (iterate
              (for index :below length)
              (let ((position
                     (apply #'array-row-major-index which
                            (mapcar (lambda (b) (aref b index)) bins))))
                (setf (row-major-aref which position) 1)
                (collect position))))
           ;; keep row-major indexes which have elements, save corresponding
           ;; subscripts
           row-major-indexes subscripts)
      (with-indexing* (dimensions index index-next
                                  :counters counters)
        (iter
          (unless (zerop (row-major-aref which index))
            (push index row-major-indexes)
            (push (copy-seq counters) subscripts))
          (until (index-next)))
        (setf row-major-indexes (coerce (nreverse row-major-indexes)
                                        'simple-fixnum-vector)
              subscripts (coerce (nreverse subscripts) 'vector)))
      ;; create hash-table for reverse mapping
      (iter
        (for row-major-index :in-vector row-major-indexes :with-index flat-index)
        (setf (gethash row-major-index table) flat-index))
      ;; reverse mapping
      (make-instance 'discrete-binned-data
                     :indexes (map 'simple-fixnum-vector
                                   (lambda (row-major-position)
                                     (gethash row-major-position table))
                                   row-major-positions)
                     :keys subscripts))))
