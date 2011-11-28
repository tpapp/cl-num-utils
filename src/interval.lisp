;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;;; An interval is an ordered pair of real numbers.  It is not
;;;; necessarily decreasing, as there can be negative intervals (eg
;;;; for reverse plots), but some functions (eg interval-containing
;;;; and interval-intersection) return positive intervals by
;;;; construction.

(defstruct (interval
             (:constructor interval (left right)))
  "An ordered pair of numbers."
  (left 0 :type real)
  (right 0 :type real))

(define-structure-let+ (interval) left right)

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

(defun interval-midpoint (interval &optional (alpha 1/2))
  "Convex combination of left and right, with alpha (defaults to 0.5)
weight on right."
  (let+ (((&interval left right) interval))
    (+ (* (- 1 alpha) left) (* alpha right))))

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
  (interval (interval-right interval) (interval-left interval)))

(defun interval-abs (interval &optional min?)
  "Return the maximum of the absolute values of the endpoints, or the minimum
if MIN?."
  (let+ (((&interval left right) interval))
    (funcall (if min? #'min #'max) (abs left) (abs right))))

(defun interval-or-nil (minimum maximum)
  "When both arguments are non-nil, return an interval, otherwise nil. "
  (when (and minimum maximum)
    (interval minimum maximum)))

(defmacro interval-including ((include) &body body)
  "Return the narrowses interval of objects which were incorporated
with (INCLUDE OBJECT) within BODY.  If the resulting interval is the empty
set, return NIL."
  (with-unique-names (minimum maximum update)
    `(let+ (,minimum
            ,maximum
            ((&flet ,update (min max)
               (if ,minimum
                   (setf ,minimum (min ,minimum min)
                         ,maximum (max ,maximum max))
                   (setf ,minimum min
                         ,maximum max))))
            ((&flet ,include (object)
               (typecase object
                 (null)
                 (real (,update object object))
                 (interval (let+ (((&interval-r/o left right) object))
                             (when (> left right) (rotatef left right))
                             (,update left right)))
                 (t (let+ (((&interval-r/o left right) (limits object)))
                      (,update left right)))))))
       ,@body
       (interval-or-nil ,minimum ,maximum))))

(defgeneric limits (object)
  (:documentation "Return the limits of an object as a weakly positive
  interval.  If there are no elements, return NIL.  NILs are ignored.")
  (:method ((x real))
    (interval x x))
  (:method ((x interval))
    x)
  (:method ((list list))
    (interval-including (include)
      (loop for element in list do (include element))))
  (:method ((array array))
    (interval-including (include)
      (loop for index below (array-total-size array)
            do (include (row-major-aref array index))))))

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
     (return (interval-or-nil max-left min-right)))))


;;; Interval manipulations

(defstruct (relative (:constructor relative (fraction)))
  "Relative sizes are in terms of width."
  (fraction nil :type real))

(defun shrink-interval (interval left
                        &optional (right left) (check-flip? t))
  "Shrink interval by given (relative) magnitudes.  When check-flip?, the
result is checked for endpoints being in a different order than the original.
Negative LEFT and RIGHT extend the interval."
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

(defstruct (spacer (:constructor spacer (&optional value)))
  "Spacers divie the leftover portion of an interval."
  (value 1 :type (real 0)))

(defun split-interval (interval divisions)
  "Split the interval using the sequence DIVISIONS, which can be nonnegative
real numbers (or RELATIVE specifications) and spacers which divide the
leftover proportionally.  If there are no spacers and the divisions don't fill
up the interval, a spacer will be added to the end.  Return a vector of
subintervals.  If DIVISIONS is NIL, (vector INTERVAL) is returned."
  (unless divisions
    (return-from split-interval (vector interval))) ; no divisions
  (let+ ((width (interval-width interval))
         (positive? (positive-interval? interval))
	 ;; (direction (cond
	 ;;              ((positive-interval? interval) 1)
	 ;;              ((negative-interval? interval) -1)
	 ;;              (t (error "interval has to be nonzero"))))
	 (spacers 0)
	 (absolute 0)
         ((&flet absolute (x)
            (assert (<= 0 x) () "Negative division ~A." x)
            (incf absolute x)
            x))
	 (divisions
           (map 'list (lambda (div)
                        (etypecase div
                          (real (absolute div))
                          (relative (absolute (* width
                                                 (relative-fraction div))))
                          (spacer (incf spacers (spacer-value div))
                           div)))
                divisions))
	 (rest (- width absolute)))
    (when (minusp rest)
      (error "Length of divisions exceeds the width of the interval."))
    (when (and (zerop spacers) (plusp rest))
      (setf spacers 1
	    divisions (nconc divisions (list (spacer 1)))))
    (let* ((left (interval-left interval))
           (spacer-unit (/ rest spacers)))
      (map 'vector (lambda (div)
		     (let* ((step (etypecase div
                                    (number div)
                                    (spacer (* spacer-unit
                                               (spacer-value div)))))
                            (right (+ left (if positive?
                                               step
                                               (- step)))))
		       (prog1 (interval left right)
			 (setf left right))))
	   divisions))))
