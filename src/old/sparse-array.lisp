;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass sparse-array ()
  ((elements :accessor elements :initarg :elements
             :initform (make-hash-table :test #'equal))
   (limits :accessor limits :initarg :limits)
   (initial-value :accessor initial-value :initarg :initial-value :initform nil))
  (:documentation "Sparse arrays are indexed by a rectilinear coordinate
 system.  Unless set, elements are left at their initial value.  If
 initial-value is a function, it is called with the subscripts to initialize
 the elements."))

(defun sparse-array-extend-limits (limits subscripts)
  "Extend limits to incorporate subscripts.  Does error checking on the length
of subscripts."
  (let ((rank (length limits)))
    (assert (= rank (length subscripts)))
    (loop :for index :below rank
          :for subscript :in subscripts
          :do (check-type subscript fixnum)
              (aif (aref limits index)
                   (progn
                     (minf (car it) subscript)
                     (maxf (cdr it) (1+ subscript)))
                   (setf (aref limits index) (cons subscript (1+ subscript)))))))

(defun sparse-array-initial-value (initial-value subscripts)
  "Initial value semantics for sparse arrays -- functions are called with
subscripts."
  (if (functionp initial-value)
      (apply initial-value subscripts)
      initial-value))

(defmethod initialize-instance :after ((sparse-array sparse-array)
                                       &key rank &allow-other-keys)
  (check-type rank (integer 0))
  (setf (limits sparse-array) (make-array rank :initial-element nil)))

(defmethod ref ((sparse-array sparse-array) &rest subscripts)
  (let+ (((&slots-r/o elements initial-value) sparse-array)
         ((&values value present?) (gethash subscripts elements)))
    (if present?
        value
        (sparse-array-initial-value initial-value subscripts))))

(defmethod (setf ref) (value (sparse-array sparse-array) &rest subscripts)
  (sparse-array-extend-limits (limits sparse-array) subscripts)
  (setf (gethash subscripts (elements sparse-array)) value))
