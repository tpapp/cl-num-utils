;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-num-utils.num=
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:num-delta
   #:*num=-tolerance*
   #:num=
   #:num=-function
   #:define-num=-with-accessors
   #:define-structure-num=))

(in-package #:cl-num-utils.num=)

(defparameter *num=-tolerance* 1d-5 "Default tolerance for NUM=.")

(defun num-delta (a b)
  "|a-b|/max(1,|a|,|b|).  Useful for comparing numbers."
  (/ (abs (- a b))
     (max 1 (abs a) (abs b))))

(defgeneric num= (a b &optional tolerance)
  (:documentation "Compare A and B for approximate equality, checking corresponding elements when applicable (using TOLERANCE).

Two numbers A and B are NUM= iff |a-b|/max(1,|a|,|b|) <= tolerance.

Unless a method is defined for them, two objects are compared with EQUALP.

Generally, methods should be defined so that two objects are NUM= if they the same class, same dimensions, and all their elements are NUM=.")
  (:method (a b &optional (tolerance *num=-tolerance*))
    (declare (ignore tolerance))
    (equalp a b))
  (:method ((a number) (b number) &optional (tolerance *num=-tolerance*))
    (<= (abs (- a b)) (* (max 1 (abs a) (abs b)) tolerance)))
  (:method ((a array) (b array) &optional (tolerance *num=-tolerance*))
    (and (equal (array-dimensions a) (array-dimensions b))
         (loop
           for index :below (array-total-size a)
           always (num= (row-major-aref a index)
                        (row-major-aref b index)
                        tolerance))))
  (:method ((a cons) (b cons) &optional (tolerance *num=-tolerance*))
    (and (num= (car a) (car b) tolerance)
         (num= (cdr a) (cdr b) tolerance)))
  (:method ((a null) (b null) &optional (tolerance *num=-tolerance*))
    (declare (ignore tolerance))
    t))

(defun num=-function (tolerance)
  "Curried version of num=, with given tolerance."
  (lambda (a b)
    (num= a b tolerance)))

(defmacro define-num=-with-accessors (class accessors)
  "Define a method for NUM=, specialized to the given class, comparing values obtained with accessors."
  `(defmethod num= ((a ,class) (b ,class)
                  &optional (tolerance *num=-tolerance*))
     (and ,@(loop for accessor in accessors
                  collect `(num= (,accessor a) (,accessor b) tolerance)))))

(defmacro define-structure-num= (structure &rest slots)
  "Define a NUM= method for the given structure, comparing the given slots."
  (check-type structure symbol)
  `(define-num=-with-accessors ,structure
       ,(loop for slot in slots
              collect (symbolicate structure "-" slot))))
