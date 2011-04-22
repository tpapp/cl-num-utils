;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric emap-dimensions (object)
  (:method ((array array))
    (array-dimensions array))
  (:method ((sequence sequence))
    (list (length sequence)))
  (:method (object)
    nil))

(defgeneric emap-next (object)
  (:method ((array array))
    (let ((index 0))
      (lambda ()
        (prog1 (row-major-aref array index)
          (incf index)))))
  (:method ((list list))
    (lambda ()
      (prog1 (car list)
        (setf list (cdr list)))))
  (:method ((sequence sequence))
    (let ((index 0))
      (lambda ()
        (prog1 (nth index sequence)
          (incf index)))))
  (:method (object)
    (constantly object)))

(defun common-dimensions (dimensions1 dimensions2)
  (cond
    ((and dimensions1 dimensions2)
     (assert (common-length (list dimensions1 dimensions2)) ()
             "Rank mismatch between dimensions ~A and ~A."
             dimensions1 dimensions2)
     (mapcar (lambda (d1 d2)
               (cond
                ((and d1 d2)
                 (assert (= d1 d2) ()
                         "Dimension mismatch between ~A and ~A.")
                 d1)
                 (d1 d1)
                 (t d2)))
             dimensions1 dimensions2))
    (dimensions1 dimensions1)
    (t dimensions2)))

(defun emap (element-type function &rest objects)
  "Map OBJECTS elementwise using FUNCTION.  If the result is an array, it has the
given ELEMENT-TYPE."
  (bind ((dimensions (reduce #'common-dimensions objects :key #'emap-dimensions))
         (next-functions (mapcar #'emap-next objects))
         ((:flet next-result ())
          (apply function (mapcar #'funcall next-functions))))
    (if dimensions
        (aprog1 (make-array dimensions :element-type element-type)
          (dotimes (index (array-total-size it))
            (setf (row-major-aref it index) (next-result))))
        (next-result))))

(defmacro define-elementwise-operation (name arglist documentation form)
  `(defun ,name ,arglist
     ,documentation
     ,form))

(define-elementwise-operation e+ (object &rest objects)
  "Elementwise +."
  (apply #'emap t #'+ object objects))

(define-elementwise-operation e- (object &rest objects)
  "Elementwise -."
  (apply #'emap t #'- object objects))

(define-elementwise-operation e* (object &rest objects)
  "Elementwise *."
  (apply #'emap t #'* object objects))

(define-elementwise-operation e/ (object &rest objects)
  "Elementwise /."
  (apply #'emap t #'/ object objects))

(define-elementwise-operation eexpt (base power)
  "Elementwise EXPT."
  (emap #'expt t base power))

(define-elementwise-operation eexp (arg)
  "Elementwise EXP."
  (emap #'exp t arg))

(define-elementwise-operation elog (arg)
  "Elementwise LOG."
  (emap #'log t arg))

(define-elementwise-operation esqrt (arg)
  "Elementwise SQRT."
  (emap #'sqrt t arg))

(defgeneric ereduce (function object &key key initial-value)
  (:method (function (array array) &key key initial-value)
    (reduce function (flatten-array array) :key key :initial-value initial-value))
  (:method (function (sequence sequence) &key key initial-value)
    (reduce function sequence :key key :initial-value initial-value)))

(defmacro define-elementwise-reduction (name function &optional 
                                        (docstring 
                                         (format nil "Elementwise ~A." function)))
  `(defun ,name (object)
     ,docstring
     (ereduce #',function object)))

(define-elementwise-reduction emax max)
(define-elementwise-reduction emin min)
