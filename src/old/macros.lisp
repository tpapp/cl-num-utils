;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun concatenate-as-string (things)
  "Concatenate THINGS, converted to string."
  (apply #'concatenate 'string (mapcar #'string things)))

(defun make-keyword+ (&rest things)
  "Build a symbol by concatenating each element of ARGS as strings,
  and intern it the KEYWORD package."
  (make-keyword (concatenate-as-string things)))

(defun gensym+ (&rest things)
  "Gensym with concatenating each element of ARGS as strings."
  (gensym (concatenate-as-string things)))

;; (defmacro define-make-symbol% (package &optional
;;                                (name (make-symbol-in package '#:make-symbol%)))
;;   "Define a MAKE-SYMBOL% that interns in PACKAGE."
;;   `(defun ,name (&rest args)
;;      ,(format nil "Build a symbol by concatenating each element of ~
;;                    ARGS as strings, and intern it in ~A." package)
;;      (intern (concatenate-as-strings args) ,package)))


(defmacro lazy-let-block ((variable init-form) &body body)
  "Building block for LAZY-LET*.  Not exported."
  (with-unique-names (value flag)
    `(let (,value ,flag)
       (symbol-macrolet ((,variable (if ,flag
                                        ,value
                                        (setf ,flag t
                                              ,value ,init-form))))
         ,@body))))

(define-with-multiple-bindings lazy-let-block
    :plural lazy-let*
    :docstring "Similar to LET*, except that the values are evaluated on
    demand.")

(defmacro unlessf (place value-form &environment environment)
  "When PLACE is NIL, evaluate VALUE-FORM and save it there."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* ,(mapcar #'list vars vals)
       (unless ,reader-form
         (let ((,(car store-vars) ,value-form))
           ,writer-form)))))

(defmacro setf-nil (place value-form &environment environment)
  "Assert that PLACE is NIL, then evaluate VALUE-FORM and save it there."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* ,(mapcar #'list vars vals)
       (assert (not ,reader-form) () "~A is already non-nil." ',place)
       (let ((,(car store-vars) ,value-form))
           ,writer-form))))

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

(defmacro expanding (&body body)
  "Expand BODY.  Useful for generating code programmatically."
  (with-gensyms (local-macro)
    `(macrolet ((,local-macro ()
                  ,@body))
       (,local-macro))))

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
