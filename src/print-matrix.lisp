;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-num-utils.print-matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:*print-precision*
   #:*print-matrix-aligned*
   #:*print-matrix-paddig*
   #:print-matrix))

(cl:in-package #:cl-num-utils.print-matrix)

(defun print-length-truncate (dimension)
  "Return values (min dimension *print-length*) and whether the constraint is binding."
  (if (or (not *print-length*) (<= dimension *print-length*))
      (values dimension nil)
      (values *print-length* t)))

(defvar *print-precision* 5
  "Number of digits after the decimal point when printing numeric matrices.")

(defun standard-numeric-formatter (x)
  "Standard formatter for matrix printing.  Respects *print-precision*, and formats complex numbers as a+bi, eg 0.0+1.0i."
  ;; ?? do we want a complex numbers to be aligned on the +, like R? I
  ;; am not sure I like that very much, and for a lot of data, I would
  ;; visualize it graphically anyhow (I hate tables of 7+ numbers in
  ;; general).  -- Tamas, 2009-sep-13
  (let ((precision *print-precision*))
    (typecase x
      (integer (format nil "~d" x))
      (real (format nil "~,vf" precision x))
      (complex (format nil "~,vf+~,vfi"
                       precision (realpart x)
                       precision (imagpart x)))
      (t (format nil "~a" x)))))

(defvar *print-matrix-aligned* t "If non-nil, characters will be aligned.")

(defvar *print-matrix-paddig* 1 "Number of spaces between columns.")

(defun print-matrix (matrix stream
                     &key (formatter #'standard-numeric-formatter)
                          (masked-fn (constantly nil)))
  "Format and print the elements of MATRIX (a 2d array) to STREAM, using *PRINT-MATRIX-PADDING* spaces between columns.

MASKED-FN is called on row and column indices.  If it returns nil, the corresponding element is formatted using FORMATTER and printed.  Otherwise, it should return a string, which is printed as is.

If *PRINT-MATRIX-ALIGNED*, columns will be right-aligned.  At most *PRINT-LENGTH* rows and columns are printed, more is indicated with ellipses (...)."
  ;; QUESTION maybe column & row labels, not a high priority at the moment
  (let+ (((&values nrow row-trunc?) (print-length-truncate (aops:nrow matrix)))
	 ((&values ncol col-trunc?) (print-length-truncate (aops:ncol matrix)))
	 (formatted-elements (make-array (list nrow ncol)))
	 (column-widths (make-array ncol :element-type 'fixnum :initial-element 0))
	 (padding (make-array *print-matrix-paddig*
                              :element-type 'character
                              :initial-element #\space))
	 (aligned? *print-matrix-aligned*))
    ;; first pass - format elements, measure width
    (dotimes (col ncol)
      (dotimes (row nrow)
	(let+ ((masked? (funcall masked-fn row col))
               (formatted-element (aif masked?
                                       it
                                       (funcall formatter (aref matrix row col))))
	       (width (length formatted-element)))
          (maxf (aref column-widths col) width)
	  (setf (aref formatted-elements row col) formatted-element))))
    ;; second pass - print
    (dotimes (row nrow)
      (when (plusp row)
        (fresh-line stream))
      (format stream "  ")
      (dotimes (col ncol)
	(when (plusp col)
	  (princ padding stream))
	(let ((elt (aref formatted-elements row col)))
	  (if aligned?
	      (format stream "~V@A" (aref column-widths col) elt)
	      (princ elt stream))))
      (when col-trunc?
	(princ padding stream)
	(princ "..." stream)))
    (when row-trunc?
      (format stream "~&..."))))
