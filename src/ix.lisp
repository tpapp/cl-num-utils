;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass ix ()
  ((keys
    :reader keys :documentation "Keys associated with positions (a hash
    table), or NIL if there are no keys." :initarg :keys
    :initform nil)
   (cumulative-sizes 
    :initarg :cumulative-sizes :documentation
    "Cumulative sizes, same length as SUB-IXS, last one equal to the total size
    of the index.")
   (sub-ixs :reader sub-ixs :initarg :sub-ixs :documentation "Vector of
   sub-indexes.  Elements may be NIL (no further sub-indexing), a
   fixnum (indexing a vector), or another IX object."))
  (:documentation "An IX instance maps a structured, recursive index to a range
  of integers, starting from 0.  Use MAKE-IX to create them, from an index
  specification."))

(defun expanded-keys (ix)
  "Return index keys as a vector.  When there are no keys corresponding to a
position, return the position is used instead."
  (bind (((:slots-r/o keys sub-ixs) ix)
         (expanded-keys (iseq (length sub-ixs) t)))
    (when keys
      (iter
        (for (key position) :in-hashtable keys)
        (setf (aref expanded-keys position) key)))
    expanded-keys))

(defmethod print-object ((ix ix) stream)
  (print-unreadable-object (ix stream :type t)
    (labels ((print-ix (ix indent)
               (bind (((:slots-r/o cumulative-sizes sub-ixs) ix)
                      ((:accessors-r/o expanded-keys) ix)
                      (indent-string (make-string indent :initial-element #\space)))
                 (iter
                   (for key :in-vector expanded-keys)
                   (for end :in-vector cumulative-sizes)
                   (for start :previous end :initially 0)
                   (for sub-ix :in-vector sub-ixs :with-index position)
                   (format stream "~%~A~A" indent-string position)
                   (when (symbolp key) (format stream " (~A)" key))
                   (if sub-ix
                       (format stream ": ~A-~A" start end)
                       (format stream ": ~A" start))
                   (when (typep sub-ix 'ix)
                     (print-ix sub-ix (+ indent 2)))))))
      (print-ix ix 2))))

(defun ix-size (ix)
  "Number of elements addressed by IX."
  (etypecase ix
    (null 1)
    (fixnum ix)
    (ix (bind (((:slots-r/o cumulative-sizes) ix))
          (aref cumulative-sizes (1- (length cumulative-sizes)))))))

(defun repeat-ix (n ix)
  "Create another index by repeating IX N times."
  (let* ((cumulative-sizes (make-array n :element-type 'fixnum))
         (size (ix-size ix)))
    (loop
      :for position :from 0 :below n
      :for cumsum :from size :by size
      :do (setf (aref cumulative-sizes position) cumsum))
    (make-instance 'ix :cumulative-sizes cumulative-sizes
                   :sub-ixs (make-array n :initial-element ix))))

(defun row-major-ix (dimensions)
  "Create an IX instance addressing a row major ordering with given
dimensions (a vector).  SUB-IXS are reused and share structure."
  (bind ((length (length dimensions))
         result)
    (iter
      (for dimension :in-vector dimensions :downfrom (1- length))
      (setf result
            (if result
                (repeat-ix dimension result)
                dimension)))
    result))


(defun make-ix (specification)
  "Create an IX from the specification.

  ix-specification = NIL | FIXNUM | list-ix-specification | VECTOR
  list-ix-specification =  ((CONS key ix-specification) | ix-specification)*

  KEYs are symbols.  NIL is a terminal specification, addressing a single
  element.  FIXNUM addresses that many elements.

  There are two convenient abbreviations:
  
  - a VECTOR is shorthand for a row-major mapping with given dimensions.  Eg 
    #(2 3 4) is equivalent to ((4 4 4) (4 4 4)).  Structure is shared, so this
    is a  memory-efficient storage mechanism.

  - a SYMBOL in a list is equivalent to (SYMBOL), ie naming a single element.

  Examples:

  (let* ((ix (make-ix '((alpha . 3) (beta) (gamma (delta . 4) (kappa . #(2 3))))))
         (numbers (iseq (ix-size ix))))
    (list
     (sub numbers (ix ix 'alpha))
     (sub numbers (ix ix 'alpha 2))
     (sub numbers (ix ix 'beta))
     (sub numbers (ix ix 'gamma))
     (sub numbers (ix ix 'gamma 'delta))
     (sub numbers (ix ix 'gamma 'kappa))
     (sub numbers (ix ix 'gamma 'kappa 1))
     (sub numbers (ix ix 'gamma 'kappa 1 0))))

  =>

  (#(0 1 2)
    2
    3
    #(4 5 6 7 8 9 10 11 12 13)
    #(4 5 6 7)
    #(8 9 10 11 12 13)
    #(11 12 13)
    11)"
  (etypecase specification
    (null nil)
    (fixnum specification)
    (vector (row-major-ix specification))
    (list (bind (keys
                 (position 0)
                 ((:flet add-key (key))
                  (unless keys (setf keys (make-hash-table :test #'eq)))
                  (when (gethash key keys)
                    (error "Duplicate key ~A in index specification ~A."
                           key specification))
                   (setf (gethash key keys) position))
                 (sub-ixs (map 'vector
                               (lambda (sub-ix-specification)
                                 ;; remove key when present
                                 (typecase sub-ix-specification
                                   (symbol ; naming a single element
                                      (add-key sub-ix-specification)
                                      (setf sub-ix-specification nil))
                                   (cons ; naming a sub-index
                                      (bind (((maybe-key . spec) 
                                              sub-ix-specification))
                                        (when (and (symbolp maybe-key) maybe-key)
                                          (add-key maybe-key)
                                          (setf sub-ix-specification spec)))))
                                 ;; process specification
                                 (aprog1 (make-ix sub-ix-specification)
                                   (incf position)))
                               specification))
                 (cumulative-size 0)
                 (cumulative-sizes (map 'simple-fixnum-vector
                                        (lambda (sub-ix)
                                          (incf cumulative-size (ix-size sub-ix))
                                          cumulative-size)
                                        sub-ixs)))
            (make-instance 'ix :keys keys :sub-ixs sub-ixs
                           :cumulative-sizes cumulative-sizes)))))

(defun ix->labels (ix)
  "Return a vector of labels for the indexed range.  Each label is a list of
 keys, addressing the corresponding fixnum."
  (etypecase ix
    (null nil)
    (fixnum (iseq ix))
    (ix (apply #'concatenate 'vector
               (map 'list
                    (lambda (sub-ix expanded-key)
                      (map 'vector 
                           (lambda (label) (cons expanded-key label))
                           (ix->labels sub-ix)))
                    (sub-ixs ix) (expanded-keys ix))))))

(defun ix->specification (ix)
  (etypecase ix
    (null nil)
    (fixnum ix)
    (ix (bind (((:accessors-r/o expanded-keys sub-ixs) ix))
          (iter
            (for expanded-key :in-vector expanded-keys)
            (for sub-ix :in-vector sub-ixs)
            (let ((specification (ix->specification sub-ix)))
              (collect 
                  (if (symbolp expanded-key)
                      (cons expanded-key specification)
                      specification))))))))

(defun ix-position (ix key)
  "Resolve the key (a position or a symbol) into a position.  No boundary
checking."
  (check-type ix ix)
  (etypecase key
    (fixnum key)
    (symbol (bind (((:slots-r/o keys) ix))
              (aif (and keys (gethash key keys))
                   it
                   (error "Key ~A not found." key))))))

(defun ix (ix &rest keys*)
  "Resolve KEYS in IX.  Return a valid index specification."
  (labels ((resolve (ix keys* acc)
             (etypecase ix
               (null
                  (assert (null keys*) () "Unused keys ~A." keys*)
                  acc)
               (fixnum
                  (if keys*
                      (bind (((key) keys*))
                        (check-type key fixnum)
                        (assert (within? 0 key ix) () "Key ~A outside [0,~A)."
                                key ix)
                        (+ acc key))
                      (si acc (+ acc ix))))
               (ix
                  (if keys*
                      (bind (((:slots-r/o keys cumulative-sizes sub-ixs) ix)
                             ((key . rest) keys*)
                             (position
                              (etypecase key
                                (fixnum key)
                                (symbol (aif (and keys 
                                                  (gethash key keys))
                                             it
                                             )))))
                        (resolve (aref sub-ixs (ix-position ix key)) rest 
                                 (+ acc (if (zerop position)
                                            0
                                            (aref cumulative-sizes (1- position))))))
                      (si acc (+ acc (ix-size ix))))))))
    (resolve ix keys* 0)))

(defmethod sub ((ix ix) &rest ranges)
  ;; also resolve keys, when given as a fixnum or a vector
  (bind (((range) ranges)
         ((:flet ix-pos (key))
          (ix-position ix key)))
    (if (typep range '(or symbol fixnum))
        (aref (sub-ixs ix) (ix-pos range))
        (make-ix (sub (ix->specification ix) (if (vectorp range)
                                                (map 'vector #'ix-pos range)
                                                range))))))

(metabang.bind::defbinding-form (:ix)
  (bind (((function &optional (variable (gensym (symbol-name function))))
          metabang-bind::variables))
    `(bind ((,variable ,values)
            ((:flet ,function (&rest rest)) (apply #'ix ,variable rest))))))
