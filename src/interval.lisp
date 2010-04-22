;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;;; An interval is an ordered pair of real numbers.  It is not
;;;; necessarily decreasing, as there can be negative intervals (eg
;;;; for reverse plots), but some functions (eg interval-containing
;;;; and interval-intersection) return positive intervals by
;;;; construction.  The most important function is range-of, which
;;;; returns an interval containing its arguments (which can be
;;;; intervals too, among other things), ignoring nil's, or nil if all
;;;; the arguments are nil.  If it encounters a forced-interval, that
;;;; replaces the combination resulting from the previous intervals.

(defstruct (interval
             (:constructor make-interval (left right)))
  "An ordered pair of numbers."
  (left 0 :type real)
  (right 0 :type real))

(metabang.bind::defbinding-form (:interval
                                 :docstring
                                 "Intervals can be accessed as (:interval left right)."
                                 :use-values-p nil)
  (bind (((left right) metabang.bind::variables))
    `(bind (((:structure/rw interval- (,left left) (,right right)) ,values)))))

(defstruct (forced-interval
             (:include interval)
             (:constructor make-forced-interval (left right)))
  "When combined using range-of, replaces the last effective union.")

(declaim (inline interval%))
(defun interval% (operation interval)
  "Call OPERATION on LEFT and RIGHT.  For syntactic convenience."
  (funcall operation (interval-left interval) (interval-right interval)))

(defun interval-diff (interval)
  "Difference between left and right."
  (- (interval-right interval) (interval-left interval)))

(defun interval-width (interval)
  "Width of the interval (absolute value)."
  (abs (interval-diff interval)))

(defun interval-midpoint (interval &optional (alpha 0.5))
  "Convex combination of left and right, with alpha (defaults to 0.5)
weight on right."
  (+ (* (- 1 alpha) (interval-left interval)) (* alpha (interval-right interval))))

(defun positive-interval-p (interval)
  "True iff the interval is positive, ie left < right."
  (interval% #'< interval))

(defun negative-interval-p (interval)
  "True iff the interval is negative, ie left > right."
  (interval% #'> interval))

(defun weakly-positive-interval-p (interval)
  "True iff the interval is positive, ie left <= right."
  (interval% #'<= interval))

(defun weakly-negative-interval-p (interval)
  "True iff the interval is negative, ie left >= right."
  (interval% #'>= interval))

(defun zero-interval-p (interval)
  "True iff the left is equal to right."
  (interval% #'= interval))

(defun flip-interval (interval)
  "Exchange left and right."
  (make-interval (interval-right interval) (interval-left interval)))

(defun make-interval-or-nil% (minimum maximum)
  "When both arguments are given, return an interval, otherwise nil. "
  (when (and minimum maximum)
    (make-interval minimum maximum)))

(defgeneric range (object)
  (:documentation "Return the range of an object as a weakly positive
  interval.  If there are no elements, return NIL.")
  (:method ((x real))
     (make-interval x x))
  (:method ((array array))
     (iter
       (for index :from 0 :below (array-total-size array))
       (for elt := (row-major-aref array index))
       (maximizing elt :into maximum)
       (minimizing elt :into minimum)
       (finally
        (return (make-interval-or-nil% minimum maximum)))))
  (:method ((list list))
     (iter
       (for elt :in list)
       (maximizing elt :into maximum)
       (minimizing elt :into minimum)
       (finally
        (return (make-interval-or-nil% minimum maximum))))))

(defun combined-range (&rest objects)
  "Return the combined range of all objects, from left to right.  An
object of type forced-interval will make replace the range calculated
so far."
  (Declare (optimize debug))
  (iter
    (with min := nil)
    (with max := nil)
    (for object :in objects)
    (acond
      ((null object))
      ((typep object 'forced-interval)
       (bind (((:interval left right) object))
         (setf min left)
         (setf max right)))
      ((range object)
       (bind (((:interval left right) it))
         (if min 
             (setf min (min min left)
                   max (max max right))
             (setf min left
                   max right)))))
    (finally
     (return (make-interval-or-nil% min max)))))

(defun interval-intersection (&rest intervals)
  "Return intersection of intervals, which is always a (weakly) positive
interval.  Intervals which are nil are silently ignored, if all intervals are
nil, nil is returned."
  ;; flip nonpositive intervals
  (iter
    (for interval :in intervals)
    (bind (((:interval left right) (aif (positive-interval-p interval)
                                        it
                                        (flip-interval it))))
      (minimizing left :into minimum)
      (maximizing right :into maximum))
    (finally
     (return (make-interval-or-nil minimum maximum)))))

;;;;  percentages, fractions and spacers - interpreted relative to the
;;;;  interval width.  A spacer divides the remaining area in the
;;;;  given proportion, effectively solving a linear
;;;;  equation. Primarily for frame manipulation.

(defstruct (fraction (:constructor fraction (value)))
  (value 1 :type (real 0 1)))

(defun fractions (&rest xs)
  "Shorthand function that returns a list of fraction objects."
  (mapcar #'fraction xs))

(defun percent (x)
  "Define a percent (converting to a fraction)."
  (fraction (/ x 100)))

(defun percents (&rest xs)
  "Shorthand function that returns a list of fraction objects."
  (mapcar #'percent xs))

(defstruct (spacer (:constructor spacer (&optional value)))
  (value 1 :type (real 0)))

(defun spacers (&rest xs)
  "Shorthand function that returns a list of spacer objects."
  (mapcar #'spacer xs))

;;;; interval splitting

(defun split-interval (interval subdivisions)
  "Split the interval using the sequence subdivisions, which can be
positive real numbers, fractions (will be interpreted as a fraction of
_total_ width) and spacers.  If there are no spacers and the
subdivisions don't fill up the interval, a spacer will be added to the
end.  Return a vector of subintervals.  If subdivisions is
nil, (vector interval) is returned."
  (unless subdivisions
    (return-from split-interval (vector interval)))
  (let* ((width (interval-width interval))
	 (direction (cond
		      ((positive-interval-p interval) 1)
		      ((negative-interval-p interval) -1)
		      (t (error "interval has to be nonzero"))))
	 (spacers 0)
	 (non-spacers 0)
	 (subdivisions (map 'list (lambda (div)
				    (etypecase div
					;; numbers just passed through
				      ((real 0)
				       (incf non-spacers div)
				       div)
				      ;; fractions are interpreted
				      (fraction
				       (let ((x (* width 
						   (fraction-value div))))
					 (incf non-spacers x)
					 x))
				      ;; spacers are passed through
				      (spacer 
				       (incf spacers 
					     (spacer-value div))
				       div)))
			    subdivisions))
	 (rest (- width non-spacers)))
    (when (minusp rest)
      (error "subdivisions exceed the width of the interval"))
    (when (and (zerop spacers) (plusp rest))
      (setf spacers 1
	    subdivisions (nconc subdivisions (list (spacer 1)))))
    (let* ((left (interval-left interval)))
      (map 'vector (lambda (div)
		     (let ((right (+ left (* direction
					     (etypecase div
					       (number div)
					       (spacer (* rest (/ (spacer-value div)
								  spacers))))))))
		       (prog1 (make-interval left right)
			 (setf left right))))
	   subdivisions))))
