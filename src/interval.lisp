;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;;; An interval is an ordered pair of real numbers.  It is not
;;;; necessarily decreasing, as there can be negative intervals (eg
;;;; for reverse plots), but some functions (eg interval-containing
;;;; and interval-intersection) return positive intervals by
;;;; construction.

(defstruct+ (interval
             (:constructor make-interval (left right)))
  "An ordered pair of numbers."
  (left 0 :type real)
  (right 0 :type real))

(defstruct (forced-interval
             (:include interval)
             (:constructor make-forced-interval (left right))
             (:conc-name interval-))
  "When combined using combined-range, replaces the last effective union.")

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

(defun positive-interval? (interval)
  "True iff the interval is positive, ie left < right."
  (interval% #'< interval))

(defun negative-interval? (interval)
  "True iff the interval is negative, ie left > right."
  (interval% #'> interval))

(defun weakly-positive-interval? (interval)
  "True iff the interval is positive, ie left <= right."
  (interval% #'<= interval))

(defun weakly-negative-interval? (interval)
  "True iff the interval is negative, ie left >= right."
  (interval% #'>= interval))

(defun zero-interval? (interval)
  "True iff the left is equal to right."
  (interval% #'= interval))

(defun flip-interval (interval)
  "Exchange left and right."
  (make-interval (interval-right interval) (interval-left interval)))

(defun interval-abs (interval &optional min?)
  "Return the maximum of the absolute values of the endpoints, or the minimum if
MIN?."
  (let+ (((&interval left right) interval))
    (funcall (if min? #'min #'max) (abs left) (abs right))))

(defun make-interval-or-nil (minimum maximum)
  "When both arguments are given, return an interval, otherwise nil. "
  (when (and minimum maximum)
    (make-interval minimum maximum)))

(defgeneric range (object)
  (:documentation "Return the range of an object as a weakly positive
  interval.  If there are no elements, return NIL.  NILs are
  ignored.")
  (:method ((x real))
     (make-interval x x))
  (:method ((array array))
     (iter
       (for index :from 0 :below (array-total-size array))
       (for elt := (row-major-aref array index))
       (when elt
         (maximizing elt :into maximum)
         (minimizing elt :into minimum))
       (finally
        (return (make-interval-or-nil minimum maximum)))))
  (:method ((list list))
     (iter
       (for elt :in list)
       (when elt
         (maximizing elt :into maximum)
         (minimizing elt :into minimum))
       (finally
        (return (make-interval-or-nil minimum maximum))))))

(defun combined-range (&rest objects)
  "Return the combined range of all objects, from left to right.  An
object of type forced-interval will make replace the range calculated
so far."
  (iter
    (with min := nil)
    (with max := nil)
    (for object :in objects)
    (flet ((update-with (interval)
             (let+ (((&interval left right) interval))
               (if min 
                   (setf min (min min left)
                         max (max max right))
                   (setf min left
                         max right)))))
      (typecase object
        (nil)
        (forced-interval
           (let+ (((&interval left right) object))
             (setf min left)
             (setf max right)))
        (interval (update-with object))
        (t (update-with (range object)))))
    (finally
     (return (make-interval-or-nil min max)))))

(defun interval-intersection (&rest intervals)
  "Return intersection of intervals, which is always a (weakly) positive
interval.  Intervals which are nil are silently ignored, if all intervals are
nil, nil is returned."
  ;; flip nonpositive intervals
  (iter
    (for interval :in intervals)
    (let+ (((&interval left right) (if (positive-interval? interval)
                                       interval
                                       (flip-interval interval))))
      (minimizing right :into min-right)
      (maximizing left :into max-left))
    (finally
     (return (make-interval-or-nil max-left min-right)))))


;;;;  percentages, fractions and spacers - interpreted relative to the
;;;;  interval width.  A spacer divides the remaining area in the
;;;;  given proportion, effectively solving a linear
;;;;  equation. Primarily for frame manipulation.

(defstruct (fraction (:constructor fraction (value)))
  (value 1 :type real))

(defun proper-fraction? (fraction)
  "Test if a fraction is proper (ie between 0 and 1, inclusive)."
  (<= 0 (fraction-value fraction) 1))

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
		      ((positive-interval? interval) 1)
		      ((negative-interval? interval) -1)
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
                                         (assert (proper-fraction? div))
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

(defun extend-interval (interval left-ext &optional
                        (right-ext left-ext))
  "Extend interval with given magnitudes and fractions, the latter
intepreted proportionally to width (see FRACTION and PERCENT)."
  (let+ (((&interval left right) interval)
         (width (- right left))
         ((&flet absolute (ext)
            (etypecase ext
              (fraction (* width (fraction-value ext)))
              (real ext)))))
    (make-interval (- left (absolute left-ext))
                   (+ right (absolute right-ext)))))
