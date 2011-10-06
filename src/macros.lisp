;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun silent (&rest arguments)
  "Make arguments vanish.  Used to avoid large displays/their cost in
benchmarking."
  (declare (ignore arguments))
  (values))

(defmacro check-types ((&rest arguments) type)
  "CHECK-TYPE for multiple places of the same type.  Each argument is either a
place, or a list of a place and a type-string."
  `(progn
     ,@(iter
         (for argument :in arguments)
         (collecting (if (atom argument)
                         `(check-type ,argument ,type)
                         (let+ (((place type-string) argument))
                           `(check-type ,place ,type ,type-string)))))))

(defmacro define-with-multiple-bindings
    (macro &key 
             (plural (intern (format nil "~aS" macro)))
             (docstring (format nil "Multiple binding version of ~(~a~)." macro)))
  "Define a version of `macro' with multiple arguments, given as a
list.  Application of `macro' will be nested.  The new name is the 
plural of the old one (generated using format by default)."
  `(defmacro ,plural (bindings &body body)
     ,docstring
     (if bindings
         `(,',macro ,(car bindings)
                    (,',plural ,(cdr bindings)
			       ,@body))
         `(progn ,@body))))

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
        (slot-accessor (intern (format nil "~A~A" conc-name slot-name))))
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
