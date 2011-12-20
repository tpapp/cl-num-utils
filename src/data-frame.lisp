;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Layouts -- generic interface
;;; 
;;; Layouts map a structured collection of objects to vectors.  Objects within
;;; the structure may be selected using lists of keys, some objects accept a
;;; partial list of keys (eg you can select a subarray from an array-layout).

(defstruct (layout-vector
            (:constructor make-layout-vector% (element-type length)))
  element-type length)

(defun make-layout-vector (element-type length)
  (make-layout-vector% (upgraded-array-element-type element-type) length))

(define-structure-let+ (layout-vector) element-type length)

(defgeneric layout-vector (object)
  (:documentation "Return the layout vector that corresponds to OBJECT."))

(defstruct (layout-selection
            (:constructor make-layout-selection (index offset specification)))
  (index nil :type fixnum :read-only t)
  (offset nil :type fixnum :read-only t)
  (specification nil :type t :read-only t))

(define-structure-let+ (layout-selection) index offset specification)

(defun build-layout-representation (specification)
  (let+ ((table (make-hash-table :test #'equal))
         (vectors (make-array 0 :adjustable t :fill-pointer t))
         ((&labels rec (keys specification)
            (if (listp specification)
                (aprog1 (mapcar (lambda+ ((key . spec))
                                  (rec (append keys (list key)) spec))
                                specification)
                  (setf (gethash keys table) it))
                (let+ (((&layout-vector element-type length)
                        (layout-vector specification))
                       ((&values index offset)
                        (aif (position element-type vectors
                                       :key #'layout-vector-element-type)
                             (let+ (((&layout-vector nil offset)
                                     (aref vectors it)))
                               (multiple-value-prog1 (values it offset)
                                 (incf offset length)))
                             (values (vector-push-extend 
                                      (make-layout-vector element-type
                                                          length)
                                      vectors)
                                     0))))
                  (aprog1 (make-layout-selection index offset specification)
                    (setf (gethash keys table) it)))))))
    (rec nil specification)
    (values table vectors)))

(defgeneric layout-leaf-ref (specification vector offset))


;;; leafs

;;;; atoms

(defstruct layout-atom
  type)

(defun layout-atom (&optional (type t))
  (make-layout-atom :type type))

(defmethod layout-vector ((layout-atom layout-atom))
  (make-layout-vector (layout-atom-type layout-atom) 1))

(defmethod layout-leaf-ref ((layout-atom layout-atom) vector offset)
  (aref vector offset))

(defmethod (setf layout-leaf-ref) (value (layout-atom layout-atom) vector
                                   offset)
  (setf (aref vector offset) value))

;;;; arrays

(defstruct layout-array
  element-type dimensions)

(defun layout-array (dimensions &optional (element-type t))
  (make-layout-array :element-type element-type :dimensions dimensions))

(defmethod layout-vector ((layout-array layout-array))
  (let+ (((&structure-r/o layout-array- element-type dimensions) layout-array))
    (make-layout-vector element-type (product dimensions))))



(defclass layout ()
  ((specification :accessor specification :initarg :specification)
   (table :accessor table :initarg :table)
   (vectors :accessor vectors :initarg :vectors)))

(defun layout (specification)
  (let+ (((&values table vectors) (build-layout-representation specification)))
    (make-instance 'layout
                   :specification specification
                   :table table
                   :vectors vectors)))

(defmethod print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type t)
    (let+ (((&slots-r/o specification table vectors) layout))
      (format stream "~&~2Twith specification ~A" specification)
      (format stream "~&~2Trepresented in")
      (loop for v across vectors
            for index from 0 do
            (let+ (((&layout-vector-r/o element-type length) v))
              (format stream "~&~4T~D: ~A ~D" index element-type length)))
      (format stream "~&~2Tusing the mapping")
      (loop for keys being the hash-key in table using (hash-value selection)
            when (layout-selection-p selection)
              do (let+ (((&layout-selection index offset length) selection))
                   (format stream "~&~4T~A -> ~D: ~D,~D" keys index offset
                           length))))))

(defun layout-lookup-selection (layout keys)
  (let+ (((&slots table) layout)
         ((&values selection present?) (gethash keys table)))
    (assert present? () "KEYS ~A not found in layout." keys)
    selection))

(defun layout-ref* (layout vectors keys)
  (let+ ((selection (layout-lookup-selection layout keys))
         ((&labels get% (selection)
            (etypecase selection
              (layout-selection 
               (let+ (((&layout-selection-r/o index offset specification)
                       selection))
                 (layout-leaf-ref specification (aref vectors index) offset)))
              (sequence (map 'list #'get% selection))))))
    (get% selection)))

(defun (setf layout-ref*) (value layout vectors keys)
  (let+ ((selection (layout-lookup-selection layout keys))
         ((&labels set% (value selection)
            (etypecase selection
              (layout-selection
               (let+ (((&layout-selection-r/o index offset specification)
                       selection))
                 (setf (layout-leaf-ref specification (aref vectors index)
                                        offset)
                       value)))
              (sequence (progn
                          (assert (length= value selection))
                          (map 'list #'set% value selection)))))))
    (set% value selection)
    value))

(defclass data-frame ()
  ((matrices :initarg :matrices :documentation "Vector of matrices.")
   (layout :initarg :layout :documentation "Layout.")))

(defmethod nrow ((df data-frame))
  (nrow (aref (slot-value df 'matrices) 0)))

(defun make-data-frame (nrow layout)
  "Create an empty data frame of the given number of rows and LAYOUT."
  (make-instance 'data-frame
                 :matrices
                 (map 'vector
                      (lambda+ ((&layout-vector element-type length))
                        (make-array (list nrow length)
                                    :element-type element-type))
                      (slot-value layout 'vectors))
                 :layout layout))

(defmethod sub ((data-frame data-frame) &rest selections)
  (let+ (((row-selection &rest keys) selections)
         (row-selection (sub-resolve-selection row-selection 
                                               (nrow data-frame) t))
         ((&slots-r/o matrices layout) data-frame)
         ((&flet get% (row)
            (layout-ref* layout (map 'vector
                                    (lambda (matrix)
                                      (subarray matrix row))
                                    matrices)
                         keys))))
    (if (fixnum? row-selection)
        (get% row-selection)
        (map 'vector #'get% row-selection))))

(defmethod (setf sub) (value (data-frame data-frame) &rest selections)
  (let+ (((row-selection &rest keys) selections)
         (row-selection (sub-resolve-selection row-selection 
                                               (nrow data-frame) t))
         ((&slots-r/o matrices layout) data-frame)
         (writer (layout-matrix-writer layout keys)))
    (if (fixnum? row-selection)
        (funcall writer value matrices row-selection)
        (progn
          (assert (length= value row-selection))
          (map 'nil (lambda (row value)
                      (funcall writer value matrices row))
               row-selection value)))
    value))
