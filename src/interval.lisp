;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; TODO: rewrite interface
;;; TODO: open/closed, general accessors LEFT, RIGHT, CLOSED-LEFT? CLOSED-RIGHT?

(defstruct interval
  "A pair of numbers designating an interval on the real line.  Using the
constructor INTERVAL, LEFT <= RIGHT is enforced."
  (left 0 :type real :read-only t)
  (right 0 :type real :read-only t))

(defstruct plusinf-interval
  "Interval [left,∞)."
  (left nil :type real :read-only t))

(defstruct minusinf-interval
  "Interval (-∞,right]."
  (right nil :type real :read-only t))

(defgeneric left (interval)
  (:documentation "Left boundary of interval.  Second value indicates whether
  the interval is closed on that end.")
  (:method ((interval interval))
    (values (interval-left interval) t))
  (:method ((interval plusinf-interval))
    (values (plusinf-interval-left interval) t))
  (:method ((interval minusinf-interval))
    (xr:-inf)))

(defgeneric right (interval)
  (:documentation "Right boundary of interval.  Second value indicates whether
  the interval is closed on that end.")
  (:method ((interval interval))
    (values (interval-right interval) t))
  (:method ((interval plusinf-interval))
    (xr:inf))
  (:method ((interval minusinf-interval))
    (values (minusinf-interval-right interval) t)))

(define-let+-expansion (&interval (left right) :value-var value :body-var body)
  "LET+ expansion for interval endpoints.  If given a list of two values, the
second value is an indicator for whether the endpoint is closed."
  (let+ (((left &optional left-closed?) (ensure-list left))
         ((right &optional right-closed?) (ensure-list right)))
    `(let+ (((&values ,left ,left-closed?) (left ,value))
            ((&values ,right ,right-closed?) (right ,value)))
       ,@body)))

;;; TODO really implement open/closed interval ends

(declaim (inline interval))
(defun interval (left right)
  "Create an INTERVAL."
  (xr:with-template (? left right)
    (assert (xr:<= left right) ())
    (cond
      ((? real real) (make-interval :left left :right right))
      ((? real xr:inf) (make-plusinf-interval :left left))
      ((? xr:-inf real) (make-minusinf-interval :right right))
      ((? xr:-inf xr:inf) (error 'not-implemented))
      (t (error 'internal-error)))))

(defun interval-length (interval)
  "Difference between left and right."
  (- (interval-right interval) (interval-left interval)))

(defun interval-midpoint (interval &optional (alpha 1/2))
  "Convex combination of left and right, with alpha (defaults to 0.5)
weight on right."
  (let+ (((&interval left right) interval))
    (+ (* (- 1 alpha) left) (* alpha right))))

(defun in-interval? (interval number)
  "Test if NUMBER is in INTERVAL (which can be NIL, designating the empty
set)."
  (and interval
       (let+ (((&interval left right) interval))
         (<= left number right))))

(defgeneric extend-interval (interval object)
  (:documentation "Return an interval that includes INTERVAL and OBJECT.  NIL
stands for the empty set.")
  (:method ((interval null) (object null))
    nil)
  (:method ((interval null) (number real))
    (interval number number))
  (:method ((interval interval) (number real))
    (let+ (((&interval left right) interval))
      (if (<= left number right)
          interval
          (interval (min left number) (max right number)))))
  (:method (interval (object interval))
    (let+ (((&interval left right) object))
      (extend-interval (extend-interval interval left) right)))
  (:method (interval (list list))
    (reduce #'extend-interval list :initial-value interval))
  (:method (interval (array array))
    (reduce #'extend-interval (flatten-array array) :initial-value interval)))

(defmacro extendf-interval (place object &environment environment)
  "Apply EXTEND-INTERVAL on PLACE using OBJECT."
  (let+ (((&with-gensyms extended))
         ((&values dummies vals new setter getter)
          (get-setf-expansion place environment))
         ((new . rest) new))
    (assert (not rest) () "Can't expand this.")
    `(let* (,@(mapcar #'list dummies vals)
            (,new ,getter)
            (,extended (extend-interval ,new ,object)))
       (if (eq ,extended ,new)
           ,extended
           (prog1 (setf ,new ,extended)
             ,setter)))))

(defun interval-hull (object)
  "Return the smallest connected interval that contains (elements in) OBJECT."
  (extend-interval nil object))

;;; Interval manipulations

(defstruct (relative (:constructor relative (fraction)))
  "Relative sizes are in terms of width."
  ;; MAKE-RELATIVE is not exported
  (fraction nil :type (real 0) :read-only t))

(defstruct (spacer (:constructor spacer (&optional weight)))
  "Spacers divide the leftover portion of an interval."
  (weight 1 :type (real 0) :read-only t))

(defun split-interval (interval divisions)
  "Return a vector of subintervals (same length as DIVISIONS), splitting the
interval using the sequence DIVISIONS, which can be nonnegative real
numbers (or RELATIVE specifications) and SPACERs which divide the leftover
proportionally.  If there are no spacers and the divisions don't fill up the
interval, and error is signalled."
  (let+ ((length (interval-length interval))
	 (spacers 0)
	 (absolute 0)
         ((&flet absolute (x)
            (incf absolute x)
            x))
	 (divisions
          (map 'vector
               (lambda (div)
                 (etypecase div
                   (real (absolute div))
                   (relative (absolute (* length (relative-fraction div))))
                   (spacer (incf spacers (spacer-weight div))
                    div)))
               divisions))
	 (rest (- length absolute)))
    (when (minusp rest)
      (error "Length of divisions exceeds the width of the interval."))
    (assert (not (and (zerop spacers) (plusp rest))) ()
            "Divisions don't use up the interval.")
    (let* ((left (interval-left interval))
           (spacer-unit (/ rest spacers)))
      (map 'vector (lambda (div)
		     (let* ((step (etypecase div
                                    (number div)
                                    (spacer (* spacer-unit
                                               (spacer-weight div)))))
                            (right (+ left step)))
		       (prog1 (interval left right)
			 (setf left right))))
	   divisions))))

(defun shrink-interval (interval left
                        &optional (right left)
                                  (check-flip? t))
  "Shrink interval by given magnitudes (which may be REAL or RELATIVE).  When
check-flip?, the result is checked for endpoints being in a different order
than the original.  Negative LEFT and RIGHT extend the interval."
  (let+ (((&interval l r) interval)
         (d (- r l))
         ((&flet absolute (ext)
            (etypecase ext
              (relative (* d (relative-fraction ext)))
              (real ext))))
         (l2 (+ l (absolute left)))
         (r2 (- r (absolute right))))
    (when check-flip?
      (assert (= (signum d) (signum (- r2 l2)))))
    (interval l2 r2)))
