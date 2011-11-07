;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric differentiate% (n method f x h)
  (:documentation "Calculate the Nth derivative of F at X, using a (relative)
stepsize H and the given METHOD.  When H is nil, a sensible default is chosen.
If there is a second value returned, it is F(X) (useful for calculating
elasticities)."))

(defun differentiate (f x &key (n 1) (method :right) h)
  (differentiate% n method f x h))

(defmethod differentiate% ((n (eql 1)) (method (eql :right)) f x h)
  (let* ((x (float x 1d0))
         (h (* (max (abs x) 1)
               (if h
                   h
                   (sqrt double-float-epsilon))))
         (fx (funcall f x)))
    (values (/ (- (funcall f (+ x h)) fx)
               h)
            fx)))

;; (defmethod differentiate% ((n (eql 1)) (method (eql :riccati-right)))
;;   (let))

;; (differentiate #'sin pi)

(defun derivative (f &key (n 1) (method :right) h)
  "Return "
  (lambda (x)
    (differentiate f x :n n :method method :h h)))

(defun elasticity (f &key (n 1) (method :right) h)
  "Return "
  (lambda (x)
    (let+ (((&values df fx) 
            (differentiate f x :n n :method method :h h)))
      (/ df fx))))
