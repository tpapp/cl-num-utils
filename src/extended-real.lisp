;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:cl-num-utils.extended-real
  (:use #:cl #:alexandria)
  (:nicknames #:xreal)
  (:shadow #:= #:< #:> #:<= #:>=)
  (:export
   :inf
   :-inf
   :inf?
   :-inf?
   :infinite?
   :extended-real
   :=
   :<
   :>
   :<=
   :>=
   :with-template
   :lambda-template))

(in-package #:cl-num-utils.extended-real)

(defun inf ()
  "Return an object representing (positive) infinity."
  :inf)

(defun -inf ()
  "Return an object representing negative infinity."
  :-inf)

(defun inf? (object)
  "Test if an object represents (positive) infinity."
  (eq :inf object))

(defun -inf? (object)
  "Test if an object represents negative infinity."
  (eq :-inf object))

(defun infinite? (object)
  "Test if an object represents positive or negative infinity."
  (or (inf? object) (-inf? object)))

(deftype extended-real (&optional (base 'real))
  `(or (satisfies inf?) (satisfies -inf?) ,base))

(defun extend-pairwise-comparison (test first rest)
  ""
  (loop while rest do
    (let ((next (car rest)))
      (unless (funcall test first next)
        (return-from extend-pairwise-comparison nil))
      (setf first next
            rest (cdr rest))))
  t)

(defmacro with-template ((prefix &rest variables) &body body)
  (let ((names (mapcar (curry #'symbolicate 'kind-) variables)))
    `(macrolet ((,prefix ,names
                  (flet ((expand (kind variable)
                           (ecase kind
                             (inf `(inf? ,variable))
                             (-inf `(-inf? ,variable))
                             (real `(realp ,variable))
                             ((t) t))))
                    (list 'and
                          ,@(mapcar (lambda (name variable)
                                      `(expand ,name ',variable))
                                    names variables)))))
       ,@(loop for v in variables
               collect `(check-type ,v extended-real))
       ,@body)))

(defmacro lambda-template ((prefix &rest variables) &body body)
  `(lambda ,variables
     (with-template (,prefix ,@variables)
       ,@body)))

(defmacro define-pairwise (name test)
  `(defun ,name (number &rest more-numbers)
     (extend-pairwise-comparison ,test number more-numbers)))

(define-pairwise =
    (lambda-template (? a b)
      (if (? real real)
          (cl:= a b)
          (or (? inf inf)
              (? -inf -inf)))))

(define-pairwise <
    (lambda-template (? a b)
      (if (? real real)
          (cl:< a b)
          (or (? -inf inf)
              (? -inf real)
              (? real inf)))))

(define-pairwise >
    (lambda-template (? a b)
      (if (? real real)
          (cl:> a b)
          (or (? inf -inf)
              (? real -inf)
              (? inf real)))))

(define-pairwise <=
    (lambda-template (? a b)
      (if (? real real)
          (cl:<= a b)
          (or (? -inf t)
              (? t inf)))))

(define-pairwise >=
    (lambda-template (? a b)
      (if (? real real)
          (cl:>= a b)
          (or (? t -inf)
              (? inf t)))))

;;; TODO /=, min, max, minusp, plusp, abs, ...
