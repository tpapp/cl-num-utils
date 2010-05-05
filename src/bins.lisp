;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Interface for binning functions
;;; 
;;; A binning function maps a value to an integer in the interval
;;; [start,end), either of which may be infinity, which is denoted by
;;; NIL.  In BIND, (:bins BINS) will allow BINS to be called as a
;;; local function.


(defgeneric bin-range (bins)
  (:documentation "Return (list START END), such that all for bin
  indexes i, START <= i < END, unless START and/or END is NIL, in
  which case the corresponding constraint does not hold."))

(defgeneric bin-index (bins value)
  (:documentation "Return the bin index (an integer) corresponding to
  value."))

(defgeneric bin-function (bins)
  (:documentation "Return a closure that can be used for binning.  The
  main purpose is optimization.  Consequences may be undefined if BINS
  is modified after calling BIN-FUNCTION.")
  (:method (bins)
    (lambda (value)
      (bin-index bins value))))

(define-condition bin-domain-error (error)
  ()
  (:documentation "This error can/should be signalled when a value is
  outside all defined bins.  Functions that use bins can provide
  restarts (eg not counting the value in a histogram, etc."))

(defmethod metabang.bind.developer:bind-generate-bindings 
    ((kind (eql :bins))
     variable-form value-form
     body declarations remaining-bindings)
  (assert value-form () "No values provided!")
  (bind (((var) variable-form)
         ((value) value-form)
         (bin-function (gensym "BIN-FUNCTION")))
    (check-type var symbol)
    `((let* ((,var ,value)
             (,bin-function (bin-function ,var)))
        (declare (type (function (real) integer) ,bin-function))
        (flet ((,var (value)
                 (funcall ,bin-function value)))
                     ,(metabang-bind::bind-filter-declarations declarations variable-form)
          ,@(metabang-bind::bind-macro-helper 
             remaining-bindings declarations body))))))

;;; Evenly distributed bins

(defclass evenly-distributed-bins ()
  ((offset :accessor offset :initarg :offset :type number
           :initform 0)
   (bin-width :accessor bin-width :initarg :bin-width :type (and number (satisfies plusp))
              :initform 1))
  (:documentation "Evenly distributed bins.  Mapping is
  right-continuous."))

(defmethod bin-range ((bins evenly-distributed-bins))
  (list nil nil))

(declaim (inline evenly-distributed-bins%))
(defun evenly-distributed-bins% (value offset bin-width)
  (floor (+ offset value) bin-width ))

(defmethod bin-index ((bins evenly-distributed-bins) value)
  (bind (((:slots-read-only offset bin-width) bins))
    (evenly-distributed-bins% value offset bin-width)))

(defmethod bin-function ((bins evenly-distributed-bins))
  (bind (((:slots-read-only offset bin-width) bins))
    (lambda (value)
      (evenly-distributed-bins% value offset bin-width))))

(defun pretty-bins (width n &key (min-step (default-min-step width))
                    (bias *pretty-bias*) (five-bias *pretty-five-bias*)) 
  "Bins with a pretty step size, calculated using PRETTY-STEP (see its
documentation)."
  (make-instance 'evenly-distributed-bins
                 :bin-width (pretty-step width n :min-step min-step :bias bias :five-bias five-bias)))

(defun integer-bins ()
  "Bins for integers."
  (make-instance 'evenly-distributed-bins :offset 0.5))

(defun sturges-bins (width length)
  "Bins with a pretty step size, n based on length of data, calculated
using Sturges's rule. "
  (pretty-bins width (1+ (ceiling (log length 2)))))

;;; Irregular bins

(defclass irregular-bins ()
  ((breaks :accessor breaks :initarg :breaks
               :type (simple-array real (*))))
  (:documentation "Irregular bondaries, bins indexed from 0.  Mapping
  is right-continuous.  After-initialization checks of breaks can
  be skipped with SKIP-COPY? and SKIP-CHECK? when calling MAKE-INSTANCE."))

(defmethod initialize-instance :after ((bins irregular-bins) &key skip-copy? skip-check?)
  (with-slots (breaks) bins
    (unless skip-copy?
      (setf breaks (coerce breaks '(simple-array * (*)))))
    (unless skip-check?
      (vector-satisfies? breaks #'<))))

(defmethod bin-range ((bins irregular-bins))
  (list 0 (1- (length (breaks bins)))))

(declaim (inline within-breaks? in-bin?% find-bin%))

(defun within-breaks? (value left right)
  "Return non-nil iff value is in [left,right)."
  (and (<= left value) (< value right)))

(defun in-bin?% (value index breaks)
  "Return non-nil iff VALUE is in the bin corresponding to INDEX.  No
error checking, for internal use."
  (within-breaks? value
                  (aref breaks index)
                  (aref breaks (1+ index))))

(defun find-bin% (value breaks right &aux (left 0))
  "Find the bin index for value.  BREAKS should be strictly
increasing.  The invariants 0 <= LEFT < RIGHT < (LENGTH BREAKS)
and (WITHIN-BREAKS? VALUE (AREF BREAKS LEFT) (AREF BREAKS RIGHT))
are maintaned and expected to be satisfied when calling this function.  For
internal use."
  (loop
    (when (= (1+ left) right)
      (return left))
    (let ((middle (floor (+ left right) 2)))
      (if (< value (aref breaks middle))
          (setf right middle)
          (setf left middle)))))

(defmethod bin-index ((bins irregular-bins) value)
  (bind (((:slots-read-only breaks) bins)
         (right (1- (length breaks)))
         (left-boundary (aref breaks 0))
         (right-boundary (aref breaks right)))
    (unless (within-breaks? value left-boundary right-boundary)
      (error 'bin-domain-error))
    (find-bin% value breaks right)))

(defmethod bin-function ((bins irregular-bins))
  (bind (((:slots-read-only breaks) bins)
         (right (1- (length breaks)))
         (left-boundary (aref breaks 0))
         (right-boundary (aref breaks right)))
    ;; ?? caching would be nice, test speed
    (lambda (value)
      (unless (within-breaks? value left-boundary right-boundary)
        (error 'bin-domain-error))
      (find-bin% value breaks right))))
