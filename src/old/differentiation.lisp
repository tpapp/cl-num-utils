;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defgeneric differentiate% (n method f x h)
  (:documentation "Calculate the Nth derivative of F at X, using a (relative)
stepsize H and the given METHOD.  When H is nil, a sensible default is chosen.
If there is a second value returned, it is F(X) (useful for calculating
elasticities)."))

(defun differentiate (f x &key (n 1) (method :right) h)
  (differentiate% n method f x h))

(defun numdiff-epsilon (x &optional h)
  "Sensible choice of epsilon for numerical differentiation."
  (* (max (abs x) 1)
     (if h
         h
         (sqrt double-float-epsilon))))

(defmethod differentiate% ((n (eql 1)) (method (eql :right)) f (x real) h)
  (let* ((x (float x 1d0))
         (h (numdiff-epsilon x h))
         (fx (funcall f x)))
    (values (/ (- (funcall f (+ x h)) fx)
               h)
            fx)))

(defun add-standard-basis-vector (x axis h)
  "Return a x+e, where e_i = h if i=axis, 0 otherwise."
  (aprog1 (copy-array x)
    (incf (aref it axis) h)))

(defmethod differentiate% ((n (eql 1)) (method (eql :right)) f (x vector) h)
  (let ((fx (funcall f x))
        (h (map 'vector (lambda (x) (numdiff-epsilon x h)) x))
        (length (length x)))
    (aprog1 (make-array length)
      (loop for axis below length
            for h across h
            do (setf (aref it axis)
                     (/ (- (funcall f (add-standard-basis-vector x axis h)) fx)
                        h))))))

;;; !!! todo: write two-sided, left, Richardson approximation, etc

(defun derivative (f &key (n 1) (method :right) h)
  "Return a function that calculates the derivative numerically.  See
DIFFERENTIATE for an explanation of the parameters."
  (lambda (x)
    (differentiate f x :n n :method method :h h)))

(defun semi-elasticity (f &key (n 1) (method :right) h)
  "Return a function that calculates the semi-elasticity numerically.  See
DIFFERENTIATE for an explanation of the parameters."
  (lambda (x)
    (let+ (((&values df fx)
            (differentiate f x :n n :method method :h h)))
      (/ df fx))))

(defun elasticity (f &key (n 1) (method :right) h)
  "Return a function that calculates the elasticity numerically.  See
DIFFERENTIATE for an explanation of the parameters."
  (lambda (x)
    (let+ (((&values df fx)
            (differentiate f x :n n :method method :h h)))
      (* df (/ x fx)))))
