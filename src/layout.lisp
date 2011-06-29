;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

;;; Layouts map a structured collection of objects to vectors.  Objects within
;;; the structure may be selected using lists of keys, some objects accept a
;;; partial list of keys (eg you can select a subarray from an array-layout).

(defgeneric layout-length (layout)
  (:documentation "Return the length of the vector that can be mapped with
  LAYOUT."))

(defgeneric layout-position (layout &rest keys)
  (:documentation "Return either a fixnum, or (cons START END) denoting the
  position of KEYS in LAYOUT.  When applicable, also return the layout that
  can resolve the subvector at this position; for fixnums the second value is
  NIL."))

(defun whole-layout-position (layout)
  "Helper function for layout-position when there are no keys."
  (values (cons 0 (layout-length layout)) layout))

(defun shift-layout-position (position offset)
  "Shift POSITION (usually returned by LAYOUT-POSITION) by OFFSET."
  (etypecase position
    (cons (cons (+ offset (car position)) (+ offset (cdr position))))
    (fixnum (+ offset position))))

(defgeneric layout-ref (vector layout &rest keys)
  (:documentation "Return object from vector, extracted using KEYS and LAYOUT.
  May share structure."))

(defgeneric (setf layout-ref) (value vector layout &rest keys)
  (:documentation "Copy elements of object into vector at offset."))

(defun flatten-using-layout (layout value &optional (element-type t))
  "Flatten VALUE into a vector using LAYOUT."
  (aprog1 (make-array (layout-length layout) :element-type element-type)
    (setf (layout-ref it layout) value)))

;;; atomic layout - denoted by NIL

(defmethod layout-length ((layout null))
  1)

(defmethod layout-position ((layout null) &rest keys)
  (assert (not keys))
  (values 0 layout))

(defmethod layout-ref (vector (layout null) &rest keys)
  (assert (not keys))
  (aref vector 0))

(defmethod (setf layout-ref) (value vector (layout null) &rest keys)
  (assert (not keys))
  (assert (= (length vector) 1))
  (setf (aref vector 0) value))

;;; array layout

(defstruct (array-layout 
             (:constructor array-layout (&rest dimensions)))
  "Layout for elements in a row-major order."
  (dimensions nil :type list))

(defmethod layout-length ((array-layout array-layout))
  (product (array-layout-dimensions array-layout)))

(defmethod layout-position ((layout array-layout) &rest keys)
  (unless keys (return-from layout-position (whole-layout-position layout)))
  (let+ (((&structure array-layout- dimensions) layout)
         (rank (length dimensions))
         (drop (length keys))
         ((&assert (<= drop rank)))
         (remaining-dimensions (subseq dimensions drop))
         (remaining-size (product remaining-dimensions))
         (offset 0))
    (iter
      (with product := remaining-size)
      (for dimension :in (nreverse (subseq dimensions 0 drop)))
      (for subscript :in (reverse keys))
      (assert (within? 0 subscript dimension))
      (incf offset (* product subscript))
      (multf product dimension))
    (if remaining-dimensions
        (values
         (cons offset (+ offset remaining-size))
         (apply #'array-layout remaining-dimensions))
        (values offset nil))))

(defmethod layout-ref (vector (layout array-layout) &rest keys)
  (apply #'subarray
         (displace-array vector
                         (array-layout-dimensions layout))
         keys))

(defmethod (setf layout-ref) (value vector (layout array-layout) &rest keys)
  (setf (apply #'subarray
               (displace-array vector (array-layout-dimensions layout))
               keys)
        value))

;;; dictionary layout

(defstruct (dictionary-layout (:constructor dictionary-layout% 
                               (keys layouts offsets test)))
  "Layout where each sub-layout is named by a symbol.  Meant to serve as a
  container for other layouts."
  keys
  layouts
  offsets
  test)

(defun dictionary-layout (alist &key (test #'equal))
  "Create a dictionary layout from a plist of layout items."
  (iter
    (with offset := 0)
    (for (key . layout) :in alist)
    (collect key :into keys :result-type vector)
    (collect layout :into layouts :result-type vector)
    (collect (incf offset (layout-length layout))
      :into offsets :result-type simple-fixnum-vector)
    (finally
     (return (dictionary-layout% keys layouts offsets test)))))

(defmethod layout-length ((layout dictionary-layout))
  (vector-last (dictionary-layout-offsets layout)))

(defun dictionary-layout-lookup (layout key)
  "Return (values start end layout) when KEY is found in a dictionary layout,
  otherwise signal an error."
  (let+ (((&structure-r/o dictionary-layout- keys offsets layouts test)
          layout)
         (index (aprog1 (position key keys :test test)
                  (assert it () "key not found")))
         (start (if (zerop index)
                    0
                    (aref offsets (1- index))))
         (end (aref offsets index)))
    (values start end (aref layouts index))))

(defmethod layout-position ((layout dictionary-layout) &rest keys)
  (if keys
      (let+ (((first &rest rest) keys)
             ((&values start nil layout)
              (dictionary-layout-lookup layout first))
             ((&values sub-position sub-layout)
              (apply #'layout-position layout rest)))
        (values (shift-layout-position sub-position start) sub-layout))
      (whole-layout-position layout)))

(defmethod layout-ref (vector (layout dictionary-layout) &rest keys)
  (if keys
      (let+ (((first &rest rest) keys)
             ((&values start end layout)
              (dictionary-layout-lookup layout first)))
        (apply #'layout-ref (subvector vector start end) layout rest))
      vector))

(defmethod (setf layout-ref) (value vector (layout dictionary-layout)
                              &rest keys)
  (if keys
      (let+ (((first &rest rest) keys)
             ((&values start end layout)
              (dictionary-layout-lookup layout first)))
        (setf (apply #'layout-ref (subvector vector start end) layout rest)
              value))
      (prog1 value
        (iter
          (for (key . val) :in value)
          (setf (layout-ref vector layout key) val)))))

;;; atomic dictionary layout

(defstruct (atomic-dictionary-layout (:constructor atomic-dictionary-layout%))
  (test nil :type function)
  (keys nil :type vector))

(defun atomic-dictionary-layout (keys &key (test #'equal))
  "Dictionary of atoms."
  (atomic-dictionary-layout% :test test :keys (coerce keys 'vector)))
  
(defmethod layout-length ((layout atomic-dictionary-layout))
  (length (atomic-dictionary-layout-keys layout)))

(defmethod layout-position ((layout atomic-dictionary-layout) &rest keys)
  (unless keys (return-from layout-position (whole-layout-position layout)))
  (let+ (((key) keys)
         ((&structure atomic-dictionary-layout- test (keys% keys)) layout))
    (aprog1 (position key keys% :test test)
      (assert it () "Key ~A not found." key))))

(defmethod layout-ref (vector (layout atomic-dictionary-layout) &rest keys)
  (aref vector (apply #'layout-position layout keys)))

(defmethod (setf layout-ref) (value vector (layout atomic-dictionary-layout)
                              &rest keys)
  (setf (aref vector (apply #'layout-position layout keys)) value))

;;; shifted vector layout

(defstruct (shifted-vector-layout
             (:constructor shifted-vector-layout (start end)))
  "A vector-like layout, indexed from something else than 0, but still mapping
to a vector."
  (start nil :type fixnum)
  (end nil :type fixnum))

(define-structure-let+ (shifted-vector-layout) start end)

(defmethod layout-length ((layout shifted-vector-layout))
  (let+ (((&shifted-vector-layout start end) layout))
    (- end start)))

(defmethod limits ((layout shifted-vector-layout))
  (let+ (((&shifted-vector-layout start end) layout))
    (cons start end)))

(defmethod layout-position ((layout shifted-vector-layout) &rest keys)
  (unless keys (return-from layout-position (whole-layout-position layout)))
  (let+ (((&shifted-vector-layout start end) layout))
    (let+ (((key) keys))
      (assert (within? start key end))
      (values (- key start) nil))))

(defmethod layout-ref (vector (layout shifted-vector-layout) &rest keys)
  (if keys
      (let+ (((&shifted-vector-layout start nil) layout)
             ((key) keys))
        (aref vector (- key start)))
      vector))

(defmethod (setf layout-ref) (value vector (layout shifted-vector-layout)
                              &rest keys)
  (if keys
      (let+ (((&shifted-vector-layout start nil) layout)
             ((key) keys))
        (setf (aref vector (- key start)) value))
      (prog1 value
        (assert (= (length vector) (length value)))
        (replace vector value))))


