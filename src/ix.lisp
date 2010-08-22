;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defclass ix ()
  ((keys :reader ix-keys :initarg :keys)
   (cum-indexes :initarg :cum-indexes)
   (specs :reader ix-specs :initarg :specs)))

(defun ix-size (ix)
  "Number of elements addressed by an IX specification."
  (etypecase ix
    (null 1)
    (fixnum ix)
    (vector (reduce #'* ix))
    (ix (with-slots (cum-indexes) ix
          (aref cum-indexes (1- (length cum-indexes)))))))

(defun flatten-ix (ix)
  "Return a vector, which contains keys for each index."
  (labels ((flatten (key spec)
             (etypecase spec
               (null (vector (list key)))
               (fixnum (iter
                         (for index :from 0 :below spec)
                         (collecting (list key index) :result-type vector)))
               (vector (error "internal error: case not defined yet"))
               (ix (map 'vector (lambda (keys)
                                  (cons key keys))
                        (flatten-ix spec))))))
    (apply #'concatenate 'vector 
           (map 'list #'flatten (ix-keys ix) (ix-specs ix)))))

(defun ix->spec (ix)
  "Return the specification for an IX object."
  (labels ((spec->list (key spec)
             (etypecase spec
               (null key)
               ((or fixnum vector) (list key spec))
               (ix (list key (ix->list spec)))))
           (ix->list (ix)
             (map 'list #'spec->list (ix-keys ix) (ix-specs ix))))
    (ix->list ix)))

(defun ix->labels (ix)
  (map 'vector (lambda (name) (format nil "~(~{~a~^ ~}~)" name))
       (flatten-ix ix)))

(defmethod print-object ((ix ix) stream)
  (print-unreadable-object (ix stream :type t)
    (princ (ix->spec ix) stream)))

(defun make-ix (specification)
  "Create index.  SPEC is a list of the following: KEY for
singletons, (KEY LENGTH) or (KEY DIMENSIONS-VECTOR) for vector or
row-major-array indexing, (KEY IX-INSTANCE), or a list of these which
is interpreted recursively."
  (iter
    (with cum-index := 0)
    (for key-spec :in specification)
    (bind (((:values key spec)
            (if (atom key-spec)
                (progn
                  (check-type key-spec symbol)
                  (values key-spec nil 1))
                (bind (((key spec) key-spec))
                  (check-type key symbol)
                  (values key
                          (typecase spec
                            (fixnum spec)
                            (vector (coerce spec 'simple-fixnum-vector))
                            (ix spec)
                            (list (make-ix spec)))))))
           (size (ix-size spec)))
      (collecting key :into keys :result-type vector)
      (collecting spec :into specs :result-type vector)
      (when (first-iteration-p)
        (collecting 0 :into cum-indexes :result-type simple-fixnum-vector))
      (incf cum-index size))
      (collecting cum-index :into cum-indexes :result-type simple-fixnum-vector)
    (finally
     (return (make-instance 'ix :specs specs :cum-indexes cum-indexes :keys keys)))))

(defun conforming-ix (instance &rest slots)
  "Return an index conforming to the slots of INSTANCE."
  (make-ix (mapcar (lambda (slot)
                     (let ((value (slot-value instance slot)))
                       (if (vectorp value)
                           `(,slot ,(length value))
                           slot)))
                   slots)))

(defun ix (ix &rest keys-and-indexes)
  "Resolve KEYS-AND-INDEXES in IX.  Return either a range
specification (eg (CONS START END)) or a single index."
  (labels ((resolve (ix keys-and-indexes acc)
             (etypecase ix
               (null
                  (assert (null keys-and-indexes))
                  acc)
               (fixnum
                  (if keys-and-indexes
                      (bind (((key) keys-and-indexes))
                        (assert (within? 0 key ix))
                        (+ acc key))
                      (cons acc (+ acc ix))))
               (vector
                  (error "not implemented yet"))
               (ix
                  (if keys-and-indexes
                      (bind (((:slots-r/o keys cum-indexes specs) ix)
                             ((key . rest) keys-and-indexes)
                             (position (position key keys :test #'eq)))
                        (unless position
                          (error "key ~A not found" key))
                        (resolve (aref specs position) rest 
                                 (+ acc (aref cum-indexes position))))
                      (cons acc (+ acc (ix-size ix))))))))
    (resolve ix keys-and-indexes 0)))

(defmethod sub ((ix ix) &rest ranges)
  ;; sub works on the specification
  (bind (((range) ranges))
    (make-ix (sub (ix->spec ix) range))))

(defun sub-ix-process% (ix-specs)
  "Process index specifications for SUB-IX and (SETF SUB-IX)."
  (iter
    (for (ix spec . rest) :on ix-specs :by #'cddr)
    (collecting (if ix
                    (apply #'ix ix (mklist spec))
                    spec))))

(defun sub-ix (object &rest ix-specs)
  "(SUB OBJECT IX1 SPEC1 IX2 SPEC2 ...) applies each index on spec, then calls
sub on the resulting coordinates.  NIL indexes just pass spec through."
  (apply #'sub object (sub-ix-process% ix-specs)))

(defun (setf sub-ix) (source target &rest ix-specs)
  "(SUB OBJECT IX1 SPEC1 IX2 SPEC2 ...) applies each index on spec, then calls
sub on the resulting coordinates.  NIL indexes just pass spec through."
  (apply #'(setf sub) source target (sub-ix-process% ix-specs)))

;; (defparameter *ix* (make-ix '((foo 3) (bar 8) baz)))
;; (ix *ix* 'foo)
;; (ix *ix* 'bar 4)
;; (ix *ix* 'baz)
