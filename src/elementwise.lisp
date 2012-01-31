;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun elementwise-float-contagion (&rest objects)
  "Return the resulting float type when types are combined using arithmetic
operations."
  (declare (optimize speed))
  (let* ((matrix (load-time-value
                  (let ((matrix (make-array `(10 10)
                                            :element-type '(integer 0 9))))
                    (dotimes (i1 10)
                      (dotimes (i2 10)
                        (multiple-value-bind (c1 f1) (floor i1 5)
                          (multiple-value-bind (c2 f2) (floor i2 5)
                            (setf (aref matrix i1 i2)
                                  (+ (max f1 f2) (* 5 (max c1 c2))))))))
                    matrix))))
    (declare (type (simple-array (integer 0 9) (10 10)) matrix))
    (if objects
        (aref #(short-float
                single-float
                double-float
                long-float
                real
                (complex short-float)
                (complex single-float)
                (complex double-float)
                (complex long-float)
                complex)
              (reduce (lambda (i1 i2) (aref matrix i1 i2)) objects
                      :key (lambda (object)
                             (cond
                               ((arrayp object)
                                (let ((type (array-element-type object)))
                                  (cond
                                    ((subtypep type 'short-float) 1)
                                    ((subtypep type 'single-float) 2)
                                    ((subtypep type 'double-float) 3)
                                    ((subtypep type 'long-float) 4)
                                    ((subtypep type 'real) 0)
                                    ((subtypep type '(complex short-float)) 6)
                                    ((subtypep type '(complex single-float)) 7)
                                    ((subtypep type '(complex double-float)) 8)
                                    ((subtypep type '(complex long-float)) 9)
                                    ((subtypep type 'complex) 5)
                                    (t (return-from elementwise-float-contagion t)))))
                               ((typep object 'short-float) 1)
                               ((typep object 'single-float) 2)
                               ((typep object 'double-float) 3)
                               ((typep object 'long-float) 4)
                               ((typep object 'real) 0)
                               ((typep object '(complex short-float)) 6)
                               ((typep object '(complex single-float)) 7)
                               ((typep object '(complex double-float)) 8)
                               ((typep object '(complex long-float)) 9)
                               ((typep object 'complex) 5)
                               (t (return-from elementwise-float-contagion t))))))
        t)))

;;; various elementwise operations

(defmacro mapping-array ((ref array &rest other) form)
  (check-type ref symbol)
  (with-unique-names (result index)
    (once-only (array)
      `(let ((,result (make-array (array-dimensions ,array)
                                  :element-type (elementwise-float-contagion
                                                 ,array ,@other))))
         (dotimes (,index (array-total-size ,result))
           (setf (row-major-aref ,result ,index)
                 (flet ((,ref (array)
                          (row-major-aref array ,index)))
                   ,form)))
         ,result))))

(defmacro define-e1 (operation
                     &key (function (symbolicate '#:e1 operation))
                          (docstring (format nil "Univariate elementwise ~A."
                                      operation)))
  "Define an univariate elementwise operation."
  (check-types (function operation) symbol)
  `(defgeneric ,function (a)
     (declare (optimize speed))
     (:documentation ,docstring)
     (:method ((a number))
       (,operation a))
     (:method ((a array))
       (mapping-array (m a) (,operation (m a))))))

(define-e1 -)
(define-e1 /)
(define-e1 log)
(define-e1 exp :function eexp)
(define-e1 sqrt :function esqrt)
(define-e1 conjugate :function econjugate)
(define-e1 square :function esquare)

(defmacro define-e2 (operation
                     &key (function (symbolicate '#:e2 operation))
                          (docstring (format nil "Bivariate elementwise ~A."
                                      operation)))
  "Define an univariate elementwise operation."
  (check-types (function operation) symbol)
  `(defgeneric ,function (a b)
     (declare (optimize speed))
     (:documentation ,docstring)
     (:method ((a number) (b number))
       (,operation a b))
     (:method ((a array) (b number))
       (mapping-array (m a b) (,operation (m a) b)))
     (:method ((a number) (b array))
       (mapping-array (m b a) (,operation a (m b))))
     (:method ((a array) (b array))
       (assert (equal (array-dimensions a) (array-dimensions b)))
       (mapping-array (m a b) (,operation (m a) (m b))))))


(define-e2 +)
(define-e2 -)
(define-e2 *)
(define-e2 /)
(define-e2 expt :function eexpt)
(define-e2 log)

(defun elog (a &optional (base nil base?))
  "Elementwise logarithm."
  (if base?
      (e2log a base)
      (e1log a)))

(defmacro define-e& (operation &key (function (symbolicate '#:e operation))
                                    (bivariate (symbolicate '#:e2 operation))
                                    (univariate (symbolicate '#:e1 operation))
                                    (docstring (format nil "Elementwise ~A."
                                                operation)))
  `(defun ,function (argument &rest more-arguments)
     ,docstring
     (if more-arguments
         (reduce #',bivariate more-arguments :initial-value argument)
         (,univariate argument))))

(define-e& + :univariate identity)
(define-e& -)
(define-e& * :univariate identity)
(define-e& /)

;; ;;; absorbing elements
;; (defmethod e2* (a (b null)) nil)
;; (defmethod e2* ((a null) b) nil)


(defgeneric ereduce (function object &key key)
  (:documentation "Elementwise reduce, traversing in row-major order.")
  (:method (function (array array) &key key)
    (reduce function (flatten-array array) :key key))
  (:method (function (sequence sequence) &key key)
    (reduce function sequence :key key))
  (:method (function object &key key)
    (reduce function (as-array object :copy? nil) :key key)))

(defmacro define-elementwise-reduction
    (name function
     &optional (docstring (format nil "Elementwise ~A." function)))
  `(defun ,name (object)
     ,docstring
     (ereduce #',function object)))

(define-elementwise-reduction emax max)
(define-elementwise-reduction emin min)

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
