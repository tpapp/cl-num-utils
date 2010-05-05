;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass histogram-counter ()
  ((bins :accessor bins :initarg :bins)
   (counts :accessor counts :initform (make-hash-table))))

(defmacro with-histogram-counter ((histogram-counter counter-function)
                                  &body body)
  "Within BODY, COUNTER-FUNCTION will increment the counter for numbers."
  (check-type counter-function symbol)
  (once-only (histogram-counter)
    `(bind (((:bins bins) (bins ,histogram-counter))
            ((:slots-read-only counts) ,histogram-counter))
       (flet ((,counter-function (number)
                (incf (gethash (bins number) counts 0))))
         ,@body))))

(defun counter (histogram-counter)
  "Return a univariate closure that bins its argument and increments
the counter of HISTOGRAM-COUNTER."
  (with-histogram-counter (histogram-counter cf)
    #'cf))

(defgeneric histogram-count (histogram-counter object)
  (:documentation "Traverse object, bin and count indexes."))

(defmethod histogram-count ((hc histogram-counter) (object number))
  (with-histogram-counter (hc cf)
    (cf object)))

(defmethod histogram-count ((hc histogram-counter) (object list))
  (with-histogram-counter (hc cf)
    (dolist (element object)
      (cf element))))

(defmethod histogram-count ((hc histogram-counter) (object vector))
  (with-histogram-counter (hc cf)
    (iter
      (for element :in-vector object)
      (cf element))))

(defun hash-key-range (hash-table)
  "Return the range of KEYs, which are supposed to be numbers."
  (iter
    (for (key value) :in-hashtable hash-table)
    (maximizing key :into maximum)
    (minimizing key :into minimum)
    (finally
     (return
       (make-interval-or-nil minimum maximum)))))

(defun hash-key-range2 (hash-table)
  "Return a pair (cons) of intervals, which are the ranges of keys in
two dimensions.  Each key is a pair of numbers, ie (KEY1 . KEY2)."
  (iter
    (for ((key1 . key2) value) :in-hashtable hash-table)
    (maximizing key1 :into maximum1)
    (minimizing key1 :into minimum1)
    (maximizing key2 :into maximum2)
    (minimizing key2 :into minimum2)
    (finally
     (return
       (cons
         (make-interval-or-nil minimum1 maximum1)
         (make-interval-or-nil minimum2 maximum2))))))

(defun hash-table->array2 (hash-table &key (element-type t) empty-element
                           (key #'identity))
  "Convert a hash-table with keys (key1 . key2) to a 2d array.  Keys
have to be integers.  Return (values array seq1 seq2), where the
latter two values are vectors.  The value is transformed using KEY."
  (declare (optimize debug))
  (bind (((range1 . range2) (hash-key-range2 hash-table))
         ((:interval start1 last1) range1)
         ((:interval start2 last2) range2)
         (result (make-array (list (- last1 start1 -1) (- last2 start2 -1))
                             :initial-element empty-element
                             :element-type element-type)))
    (iter
      (for ((key1 . key2) value) :in-hashtable hash-table)
      (check-type key1 integer)
      (check-type key2 integer)
      (setf (aref result (- key1 start1) (- key2 start2))
            (funcall key value)))
    (values result
            (numseq start1 last1 :type 'integer)
            (numseq start2 last2 :type 'integer))))

;; (defclass histogram ()
;;   ((breaks :accessor breaks :initarg :breaks)
;;    (counts :accessor counts :initarg :counts))
;; )
