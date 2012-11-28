;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; TODO: rewrite interface
;;; TODO: open/closed, general accessors LEFT, RIGHT, CLOSED-LEFT? CLOSED-RIGHT?


;;; basic interval definitions and interface

(defgeneric left (interval)
  (:documentation "Left endpoint of interval."))

(defgeneric open-left? (interval)
  (:documentation "True iff the left endpoint of the interval is open."))

(defgeneric right (interval)
  (:documentation "Right endpoint of interval."))

(defgeneric open-right? (interval)
  (:documentation "True iff the right endpoint of the interval is open."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&interval (left right) :value-var value :body-var body)
    "LET+ expansion for interval endpoints.  If given a list of two values,
the second value is an indicator for whether the endpoint is open."
    (let+ (((left &optional (open-left? nil left-open-p)) (ensure-list left))
           ((right &optional (open-right? nil right-open-p)) (ensure-list right)))
      `(let+ ((,left (left ,value))
              ,@(splice-when left-open-p `(,open-left? (open-left? ,value)))
              (,right (right ,value))
              ,@(splice-when right-open-p `(,open-right? (open-right? ,value))))
         ,@body))))

;;; mix-in classes.  used as building blocks, none of these are exported.

(defclass interval/finite-left ()
  ((left :type real :initarg :left :reader left)
   (open-left? :type boolean :initarg :open-left? :reader open-left?))
  (:documentation "Interval with left endpoint."))

(defclass interval/finite-right ()
  ((right :type real :initarg :right :reader right)
   (open-right? :type boolean :initarg :open-right? :reader open-right?))
  (:documentation "Interval with right endpoint."))

(defclass interval/infinite-left ()
  ()
  (:documentation "Left endpoint is -∞."))

(defmethod left ((interval interval/infinite-left))
  (xr:-inf))

(defmethod open-left? ((interval interval/infinite-left))
  t)

(defclass interval/infinite-right ()
  ()
  (:documentation "Right endpoint is ∞."))

(defmethod right ((interval interval/infinite-right))
  (xr:inf))

(defmethod open-right? ((interval interval/infinite-right))
  t)

(defgeneric print-left-endpoint (interval stream)
  (:method ((interval interval/finite-left) stream)
    (let+ (((&slots-r/o left open-left?) interval))
      (format stream "~C~A" (if open-left? #\( #\[) left)))
  (:method ((interval interval/infinite-left) stream)
    (format stream "(-∞")))

(defgeneric print-right-endpoint (interval stream)
  (:method ((interval interval/finite-right) stream)
    (let+ (((&slots-r/o right open-right?) interval))
      (format stream "~A~C" right (if open-right? #\) #\]))))
  (:method ((interval interval/infinite-right) stream)
    (format stream "∞)")))


;;; interval types

(defclass interval ()
  ()
  (:documentation "Abstract superclass for all intervals."))

(defmethod print-object ((interval interval) stream)
  (print-unreadable-object (interval stream :type t)
    (print-left-endpoint interval stream)
    (format stream ",")
    (print-right-endpoint interval stream)))

(defclass finite-interval (interval interval/finite-left interval/finite-right)
  ()
  (:documentation "Interval with finite endpoints."))

(defmethod initialize-instance :after ((interval finite-interval)
                                       &key &allow-other-keys)
  (let+ (((&slots-r/o left right open-left? open-right?) interval))
    (cond
      ((> left right) (error "Intervals with LEFT > RIGHT are not allowed."))
      ((= left right) (assert (not (or open-left? open-right?)) ()
                              "Zero-length intervals cannot be (half-)open.")))))

(defclass plusinf-interval (interval interval/finite-left interval/infinite-right)
  ()
  (:documentation "Interval from LEFT to ∞."))

(defclass minusinf-interval (interval/infinite-left interval/finite-right)
  ()
  (:documentation "Interval from -∞ to RIGHT."))

(defclass real-line (interval interval/infinite-left interval/infinite-right)
  ()
  (:documentation "Representing the real line (-∞,∞)."))

(defmethod == ((a real-line) (b real-line)
                &optional (tolerance *==-tolerance*))
  (declare (ignore tolerance))
  t)

(defmethod == ((a finite-interval) (b finite-interval)
               &optional (tolerance *==-tolerance*))
  (let+ (((&interval (al alo?) (ar aro?)) a)
         ((&interval (bl blo?) (br bro?)) b))
    (and (== al bl tolerance)
         (== ar br tolerance)
         (eq alo? blo?)
         (eq aro? bro?))))


;;; interval creation interface

(declaim (inline interval))
(defun interval (left right &key open-left? open-right?)
  "Create an INTERVAL."
  (xr:with-template (? left right)
    (cond
      ((? real real) (make-instance 'finite-interval :left left :right right
                                                     :open-left? open-left?
                                                     :open-right? open-right?))
      ((? real xr:inf) (make-instance 'plusinf-interval :left left
                                                        :open-left? open-left?))
      ((? xr:-inf real) (make-instance 'minusinf-interval :right right
                                                          :open-right? open-right?))
      ((? xr:-inf xr:inf) (make-instance 'real-line))
      (t (error 'internal-error)))))

(defun plusminus-interval (center half-width
                           &key open-left? (open-right? open-left?))
  "A symmetric interval around CENTER."
  (assert (plusp half-width))
  (make-instance 'finite-interval
                 :left (- center half-width) :open-left? open-left?
                 :right (+ center half-width) :open-right? open-right?))


;;; interval

;;; FIXME code below dows not handle open/infinite endpoints

(defun interval-length (interval)
  "Difference between left and right."
  (- (right interval) (left interval)))

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
    (let* ((left (left interval))
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

(defun grid-in (interval size &optional (sequence-type nil sequence-type?))
    "Return an arithmetic sequence of the given size (length) between the
endpoints of the interval.  The endpoints of the sequence coincide with the
respective endpoint of the interval iff it is closed.  RESULT-TYPE determines
the result type (eg list), if not given it is a simple-array (of rank 1),
narrowing to the appropriate float type or fixnum if possible."
  (check-type interval finite-interval)
  (check-type size (integer 2))
  (let+ (((&interval (left open-left?) (right open-right?)) interval)
         ;; correction calculations take care of numeric contagion
         (left-correction (if open-left? 1/2 0))
         (right-correction (if open-right? 1/2 0))
         (size-1 (1- size))
         (step (/ (- right left)
                  (+ size-1 left-correction right-correction)))
         (left (+ left (* step left-correction)))
         (right (- right (* step right-correction)))
         ;;
         (step (/ (- right left) size-1))
         (element-type (cond
                         ((and sequence-type? (subtypep sequence-type 'array))
                          (let+ (((&ign &optional (element-type t) &rest &ign)
                                  sequence-type))
                            element-type))
                         ((floatp step) (type-of step))
                         ((and (fixnum? left) (fixnum? right) (fixnum? step))
                          'fixnum)
                         (t t)))
         (sequence-type (if sequence-type?
                            sequence-type
                            `(simple-array ,element-type (*)))))
    (generate-sequence sequence-type size
                       (let ((index 0))
                         (lambda ()
                           (prog1 (if (= index size-1)
                                      right
                                      (+ left (* index step)))
                             (incf index)))))))
