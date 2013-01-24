;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defmacro define-structure-slot-accessor
    (accessor structure
     &key (conc-name (format nil "~A-" structure))
          (slot-name accessor) lambda-list-rest
          (read-only? nil))
  "Define a method for the generic function ACCESSOR that acts as an accessor
for a slot in an instance of STRUCTURE.  "
  (let ((lambda-list `((instance ,structure) ,@lambda-list-rest))
        (slot-accessor (symbolicate conc-name slot-name)))
    `(progn
       (defmethod ,accessor ,lambda-list
         (,slot-accessor instance))
       ,@(unless read-only?
           `((defmethod (setf ,accessor) ,(cons 'value lambda-list)
               (setf (,slot-accessor instance) value)))))))

(defmacro with-doubles (bindings &body body)
  "Coerces value to DOUBLE-FLOAT, and binds it to VAR in (VAR VALUE) bindings.
If the binding is a symbol, or VALUE is missing, VAR will be used instead.
All variables are declared DOUBLE-FLOAT in the body."
  (let ((bindings (mapcar (lambda (binding)
                            (let+ (((variable &optional (value variable))
                                    (if (atom binding)
                                        (list binding binding)
                                        binding)))
                              (check-type variable (and symbol (not null)))
                              `(,variable (coerce ,value 'double-float))))
                          bindings)))
    `(let ,bindings
       (declare (type double-float ,@(mapcar #'first bindings)))
       ,@body)))
