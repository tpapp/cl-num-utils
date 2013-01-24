;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun all-float-types ()
  "Return a list of float types."
  '(short-float single-float double-float long-float))

(defun available-float-type? (type)
  "Return T iff type is available as a specialized array element type."
  (equal type (upgraded-array-element-type type)))

(defun array-float-types ()
  "Return a list of float types which are upgraded to themselves.
Consequences are undefined if modified."
  (load-time-value
   (remove-if (complement #'available-float-type?) (all-float-types))))

(defun array-float-and-complex-types ()
  "Return a list of float types which are upgraded to themselves.
Consequences are undefined if modified."
  (load-time-value
   (remove-if (complement #'available-float-type?)
              (append (all-float-types)
                      (mapcar (lambda (type) `(complex ,type))
                              (all-float-types))))
   t))



(defun recognized-float-types ()
  (let ((float '(short-float single-float double-float long-float)))
    (concatenate 'vector float
                 (mapcar (curry #'list 'complex) float))))

(macrolet ((define% ()
             `(defun float-type-index (type)
                (cond
                  ,@(let ((index 0))
                      (map 'list (lambda (type)
                                   (prog1 `((subtypep type ',type) ,index)
                                     (incf index)))
                           (recognized-float-types)))
                  (t nil)))))
  (define%))

(defun float-contagion-matrix ()
  (let ((indexes (ivec (length (recognized-float-types)))))
    (outer* indexes indexes
            (lambda (i1 i2)
              ))))

(defun float-contagion (&rest types)
  (declare (optimize speed))
  (let ((matrix (load-time-value
                 (let ((matrix (make-array '(8 8)
                                           :element-type '(integer 0 7))))
                   (dotimes (i1 8)
                     (dotimes (i2 8)
                       (multiple-value-bind (c1 f1) (floor i1 4)
                         (multiple-value-bind (c2 f2) (floor i2 4)
                           (setf (aref matrix i1 i2)
                                 (+ (max f1 f2) (* 4 (max c1 c2))))))))
                   matrix))))
    (declare (type (simple-array (integer 0 7) (8 8)) matrix))
    (if types
        (aref #(short-float
                single-float
                double-float
                long-float
                (complex short-float)
                (complex single-float)
                (complex double-float)
                (complex long-float))
              (reduce (lambda (i1 i2) (aref matrix i1 i2)) types
                      :key (lambda (type)
                             (cond
                               ((subtypep type 'short-float) 0)
                               ((subtypep type 'single-float) 1)
                               ((subtypep type 'double-float) 2)
                               ((subtypep type 'long-float) 3)
                               ((subtypep type '(complex short-float)) 4)
                               ((subtypep type '(complex single-float)) 5)
                               ((subtypep type '(complex double-float)) 6)
                               ((subtypep type '(complex long-float)) 7)
                               (t (return-from float-contagion t))))))
        nil)))



(defmacro define-float-contagion ()
  )

(defun float-contagion (type1 type2)
  (let+ (()
         ((&labels classify (type)
            (cond
              ((subtypep type 'complex) (values (classify ())))
              )
            (typecase type
              (complex )
              (float ))
            )
                   )))
  )

(defmacro define-arithmetic-contagion (function float-types
                                       &optional (docstring ""))
  "Define (FUNCTION TYPES) which returns the result type applying float and
complex contagion rules to TYPES, considering FLOAT-TYPES and their complex
counterparts.  For types outside these, T is returned."
  (let+ (((&flet map-types (function)
            (loop for type in float-types
                  for index from 0
                  collect (funcall function type index))))
         ((&macrolet amap-types (form)
            `(map-types (lambda (type index) ,form)))))
    `(defun ,function (types)
       ,docstring
       (declare (optimize speed))
       (let ((complex? nil)
             (float 0))
         (declare (type fixnum float))
         (loop for type in types do
           (let+ (((&values f c?)
                   (cond
                     ,@(amap-types `((subtypep type '(complex ,type))
                                     (values ,index t)))
                     ,@(amap-types `((subtypep type ',type) ,index))
                     (t (return-from ,function t)))))
             (maxf float f)
             (setf complex? (or complex? c?))))
         (if complex?
             (case float ,@(amap-types `(,index '(complex ,type))))
             (case float ,@(amap-types `(,index ',type))))))))

(define-arithmetic-contagion array-arithmetic-contagion
    #.(array-float-types)
    "Return the upgraded element type of the arguments, applying rules of
    float and complex contagion.")

(array-arithmetic-contagion '(double-float (complex single-float)))

