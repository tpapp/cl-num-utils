;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-num-utils.elementwise
  (:use #:cl
        #:alexandria
        #:cl-num-utils.arithmetic
        #:cl-num-utils.utilities
        #:let-plus)
  (:export
   #:elementwise-float-contagion
   #:e+
   #:e-
   #:e*
   #:e/
   #:e2+
   #:e2-
   #:e2*
   #:e2/
   #:e1-
   #:e1/
   #:eexpt
   #:eexp
   #:elog
   #:esqrt
   #:econjugate
   #:ereduce
   #:emin
   #:emax))

(cl:in-package #:cl-num-utils.elementwise)

(defun elementwise-float-contagion (&rest objects)
  "Return the resulting float type when objects (or their elements) are combined using arithmetic operations."
  (let* ((matrix (load-time-value
                  (let ((matrix (make-array `(10 10)
                                            :element-type '(integer 0 9))))
                    (dotimes (i1 10)
                      (dotimes (i2 10)
                        (let+ (((&values c1 f1) (floor i1 5))
                               ((&values c2 f2) (floor i2 5)))
                          (setf (aref matrix i1 i2)
                                (+ (max f1 f2) (* 5 (max c1 c2)))))))
                    matrix))))
    (declare (type (simple-array (integer 0 9) (10 10)) matrix))
    (if objects
        (aref #(real
                short-float
                single-float
                double-float
                long-float
                complex
                (complex short-float)
                (complex single-float)
                (complex double-float)
                (complex long-float))
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

(defgeneric ereduce (function object &key key)
  (:documentation "Elementwise reduce, traversing in row-major order.")
  (:method (function (array array) &key key)
    (reduce function (aops:flatten array) :key key))
  (:method (function (sequence sequence) &key key)
    (reduce function sequence :key key))
  (:method (function object &key key)
    (reduce function (aops:as-array object) :key key)))

(defmacro define-elementwise-reduction
    (name function
     &optional (docstring (format nil "Elementwise ~A." function)))
  `(defun ,name (object)
     ,docstring
     (ereduce #',function object)))

(define-elementwise-reduction emax max)
(define-elementwise-reduction emin min)
