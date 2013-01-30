;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)


;; ;;; absorbing elements
;; (defmethod e2* (a (b null)) nil)
;; (defmethod e2* ((a null) b) nil)


;;; stack
;;;
;;; In order to extend STACK for other objects, define methods for
;;; STACK-DIMENSIONS, STACK-ELEMENT-TYPE and STACK-INTO.

(defgeneric stack-dimensions (h? object)
  (:documentation "Return (cons unified-dimension other-dimension), where
  unified-dimension can be NIL.  If H?, stacking is horizontal, otherwise
  vertical.")
  (:method (h? object)
    (cons nil 1))
  (:method (h? (vector vector))
    (declare (ignore h?))
    (cons (length vector) 1))
  (:method (h? (array array))
    (let+ (((nrow ncol) (array-dimensions array)))
      (if h?
          (cons nrow ncol)
          (cons ncol nrow)))))

(defgeneric stack-element-type (object)
  (:method (object)
    (type-of object))
  (:method ((array array))
    (array-element-type array))
  (:method ((list list))
    t))

(defgeneric stack-into (object h? result cumulative-index)
  (:documentation "Used by STACK to copy OBJECT to RESULT, starting at
  CUMULATIVE-INDEX (if H?, this is the column index, otherwise the vector
  index).")
  ;; atom
  (:method (atom (h? (eql nil)) result cumulative-index)
    ;; stack vertically
    (loop
      with atom = (coerce atom (array-element-type result))
      for result-index from (array-row-major-index result cumulative-index 0)
      repeat (array-dimension result 1)
      do (setf (row-major-aref result result-index) atom)))
  (:method (atom (h? (eql t)) result cumulative-index)
    ;; stack horizontally
    (let+ ((atom (coerce atom (array-element-type result)))
           ((nrow ncol) (array-dimensions result)))
      (when (plusp nrow)
        (loop
          repeat nrow
          for result-index :from
                           (array-row-major-index result 0 cumulative-index)
            by ncol
          do (setf (row-major-aref result result-index) atom)))))
  ;; vector
  (:method ((vector vector) (h? (eql nil)) result cumulative-index)
    ;; stack vertically
    (loop
      with element-type = (array-element-type result)
      for result-index from (array-row-major-index result cumulative-index 0)
      for v across vector
      repeat (array-dimension result 1)
      do (setf (row-major-aref result result-index) (coerce v element-type))))
  (:method ((vector vector) (h? (eql t)) result cumulative-index)
    ;; stack horizontally
    (let+ ((element-type (array-element-type result))
           ((nrow ncol) (array-dimensions result)))
      (when (plusp nrow)
        (loop
          for result-index :from
                           (array-row-major-index result 0 cumulative-index)
            by ncol
          for v across vector
          repeat nrow
          do (setf (row-major-aref result result-index)
                   (coerce v element-type))))))
  ;; array
  (:method ((array array) (h? (eql nil)) result cumulative-index)
    ;; stack vertically
    (let* ((element-type (array-element-type result)))
      (loop
        for result-index :from
                         (array-row-major-index result  cumulative-index 0)
        for array-index :from 0 :below (array-total-size array)
        do (setf (row-major-aref result result-index)
                 (coerce (row-major-aref array array-index) element-type)))))
  (:method ((array array) (h? (eql t)) result cumulative-index)
    ;; stack horizontally
    (let+ ((element-type (array-element-type result))
           ((nrow ncol) (array-dimensions array))
           (offset (- (array-dimension result 1) ncol))
           (result-index (array-row-major-index result 0 cumulative-index))
           (array-index 0))
      (loop repeat nrow do
        (loop repeat ncol do
          (setf (row-major-aref result result-index)
                (coerce (row-major-aref array array-index) element-type))
          (incf result-index)
          (incf array-index))
        (incf result-index offset)))))

(defun vector-direction-horizontal? (direction)
  "Interpret vector direction, allowing for synonyms."
  (ecase direction
    ((:h :horizontal) t)
    ((:v :vertical) nil)))

(defun stack* (element-type direction objects)
  "Stack OBJECTS (a sequence) into an array with given ELEMENT-TYPE (NIL means
figuring out the type automatically).  Directions can be :VERTICAL (:V)
or :HORIZONTAL (:H)."
  (let+ ((h? (vector-direction-horizontal? direction))
         (dimensions (map 'list (curry #'stack-dimensions h?) objects))
         ((unified-dimension . other-dimension)
          (reduce (lambda (d1 d2)
                    (let+ (((u1 . o1) d1)
                           ((u2 . o2) d2))
                      (cons (if u1
                                (prog1 u1
                                  (when u2
                                    (assert (= u1 u2) ()
                                            "Dimension mismatch between ~A and ~A."
                                            u1 u2)))
                                u2)
                            (+ o1 o2))))
                  dimensions))
         (result (make-array (if h?
                                 (list unified-dimension other-dimension)
                                 (list other-dimension unified-dimension))
                             :element-type (aif element-type
                                                it
                                                (common-array-element-type
                                                 objects :key #'stack-element-type))))
         (cumulative-index 0))
    (map nil
         (lambda (object dimension)
           (stack-into object h? result cumulative-index)
           (incf cumulative-index (cdr dimension)))
         objects dimensions)
    result))

(defun stack (element-type direction &rest objects)
  "Stack OBJECTS into an array with given ELEMENT-TYPE (NIL means figuring out
the type automatically).  Directions can be :VERTICAL (:V)
or :HORIZONTAL (:H)."
  (stack* element-type direction objects))

(defun concat* (element-type sequences)
  "Concatenate SEQUENCES into to a single vector with given ELEMENT-TYPE.
Lists are treated as SIMPLE-VECTORS."
  (let* ((vectors (map 'list (lambda (v)
                               (etypecase v
                                 (vector v)
                                 (list (coerce v 'simple-vector))))
                       sequences))
         (element-type (aif element-type
                            it
                            (common-array-element-type vectors)))
         (result (make-array (reduce #'+ vectors :key #'length)
                             :element-type element-type))
         (target-index 0))
    (map nil
         (lambda (source)
           (map nil (lambda (elt)
                      (setf (aref result target-index)
                            (coerce elt element-type))
                      (incf target-index))
                source))
         vectors)
    result))

(defun concat (element-type &rest sequences)
  "Concatenate VECTORS in to a single vector with given ELEMENT-TYPE.  Lists
are treated as SIMPLE-VECTORS."
  (concat* element-type sequences))
