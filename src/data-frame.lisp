;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass data-frame ()
  ((elements :accessor elements :initarg :elements)
   (layout :accessor layout :initarg :layout)))

;; (defmacro define-column-index-function (function &optional args)
;;  `(defmethod ,function ((data-frame data-frame) ,@args)
;;      (,function (data-frame-column-index data-frame) ,@args)))

(defmethod as-array ((data-frame data-frame) &key copy?)
  (maybe-copy-array (elements data-frame) copy?))

(defun make-data-frame (elements layout &key copy?)
  "Create a data frame using ELEMENTS (a matrix) and a LAYOUT.  COPY? forces
  copying of the MATRIX, otherwise may share structure."
  (let+ (((nil ncol) (array-dimensions elements)))
    (assert (= (layout-length layout) ncol))
    (make-instance 'data-frame
                   :elements (maybe-copy-array elements copy?)
                   :layout layout)))

;; (defmacro data-frame-with-resolved-index-specification
;;     ((((matrix column-index) data-frame)
;;       ((is0 is1) index-specifications))
;;      &body body)
;;   "Wrapper macro for resolving column index specifications."
;;   (check-type matrix symbol)
;;   (check-type column-index symbol)
;;   (check-type is0 symbol)
;;   (check-type is1 symbol)
;;   (once-only (data-frame)
;;     `(let+ (((&structure data-frame- (,matrix matrix)
;;                          (,column-index column-index)) data-frame)
;;             ((,is0 ,is1) ,index-specifications)
;;             (,is1 (resolve-ix-index-specification
;;                    (data-frame-column-index ,data-frame) ,is1)))
;;        (declare (ignorable ,matrix ,column-index ,is0 ,is1))
;;        ,@body)))

;; (defmethod sub ((data-frame data-frame) &rest index-specifications)
;;   (data-frame-with-resolved-index-specification 
;;       (((matrix column-index) data-frame)
;;        ((is0 is1) index-specifications))
;;     (let ((sub-matrix (sub matrix is0 is1)))
;;       (if (vectorp sub-matrix)
;;           sub-matrix
;;           (make-data-frame% sub-matrix (sub column-index is1))))))

;; (defmethod (setf sub) ((array array) (data-frame data-frame)
;;                        &rest index-specifications)
;;   (data-frame-with-resolved-index-specification
;;       (((matrix column-index) data-frame)
;;        ((is0 is1) index-specifications))
;;     (setf (sub matrix is0 is1) array)))

;; (defmethod filter-rows (predicate (data-frame data-frame))
;;   (let+ (((&structure data-frame- matrix column-index) data-frame))
;;     (make-data-frame% (filter-rows predicate matrix) column-index)))

;; (defmacro with-filter-data-frame (data-frame (&rest name-key-pairs) &body body)
;;   "Similar to WITH-FILTER-ROWS, but also allows keys.  If the latter is not
;; given, the quoted name will be used instead."
;;   (with-unique-names (resolve index)
;;     (let ((name-column-pairs
;;            (mapcar (lambda (name-key-pair)
;;                      (let+ (((name &optional (key `',name))
;;                              (alexandria:ensure-list name-key-pair)))
;;                        `(,name (,resolve ,key))))
;;                    name-key-pairs)))
;;       (once-only (data-frame)
;;         `(let ((,index (data-frame-column-index ,data-frame)))
;;            (flet ((,resolve (key)
;;                     (if (typep key 'fixnum)
;;                         key
;;                         (ix ,index key))))
;;              (with-filter-rows ,data-frame ,name-column-pairs
;;                ,@body)))))))

;; (defmethod map-columns (function (data-frame data-frame))
;;   (bind (((:structure data-frame- matrix column-index) data-frame)
;;          (result (map-columns function matrix)))
;;     (if (vectorp result)
;;         result
;;         (make-data-frame% result column-index))))

;; (defmethod shrink-rows ((data-frame data-frame) &key (predicate #'identity))
;;   (bind (((:structure data-frame- matrix column-index) data-frame)
;;          ((:values matrix start end) (shrink-rows matrix :predicate predicate)))
;;     (make-data-frame% matrix (sub column-index (si start end)))))

;;; !! maybe write compiler macro for
;;; !! (setf (sub data-frame ..) (matrix data-frame ...))
