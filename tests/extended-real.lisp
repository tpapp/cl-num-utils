(in-package #:cl-num-utils-tests)

(defsuite extended-real-tests (tests))

;;; helper macros for defining tests

(defun assert-relation (relation &rest argument-lists)
  "Assert RELATION called with each set of arguments."
  (loop for a in argument-lists
        do (assert-true (apply relation a))))

(defun assert-not-relation (relation &rest argument-lists)
  "Assert that RELATION does not hold, called with each set of arguments."
  (loop for a in argument-lists
        do (assert-false (apply relation a))))

(defun assert-paired-relation (relation1 relation2 &rest argument-lists)
  (apply #'assert-relation relation1 argument-lists)
  (apply #'assert-relation relation2 (mapcar #'reverse argument-lists)))

(defun assert-not-paired-relation (relation1 relation2 &rest argument-lists)
  (apply #'assert-not-relation relation1 argument-lists)
  (apply #'assert-not-relation relation2 (mapcar #'reverse argument-lists)))

(defun assert-relation-corner-cases (&rest relations)
  (loop for r in relations
        do (assert-true (funcall r 1))
           (assert-true (funcall r :plusinf))
           (assert-true (funcall r :minusinf))
           (assert-condition error (funcall r))))

(deftest relation-corner-cases-test (extended-real-tests)
  (assert-relation-corner-cases #'xreal:= #'xreal:< #'xreal:> #'xreal:>= #'xreal:<=))

(deftest strict-inequalities-test (extended-real-tests)
  (assert-paired-relation #'xreal:< #'xreal:>
                          ;; < pairs
                          '(1 2)
                          '(1 :plusinf)
                          '(:minusinf :plusinf)
                          '(:minusinf 1)
                          ;; < sequences
                          '(1 2 3)
                          '(1 2 :plusinf)
                          '(:minusinf 1 4 :plusinf))
  (assert-not-paired-relation #'xreal:< #'xreal:>
                              ;; not < pairs
                              '(1 1)
                              '(2 1)
                              '(:plusinf :plusinf)
                              '(:plusinf 1)
                              '(:minusinf :minusinf)
                              '(:plusinf :minusinf)
                              '(1 :minusinf)
                              ;; not < sequences
                              '(1 2 2)
                              '(1 3 2)
                              '(1 :plusinf 2)
                              '(1 :plusinf :plusinf)))

(deftest inequalities-test (extended-real-tests)
  (assert-paired-relation #'xreal:<= #'xreal:>=
                          ;; <= pairs
                          '(1 1)
                          '(1 2)
                          '(1 :plusinf)
                          '(:plusinf :plusinf)
                          '(:minusinf :plusinf)
                          '(:minusinf :minusinf)
                          '(:minusinf 1)
                          ;; < sequences
                          '(1 2 2)
                          '(1 2 3)
                          '(1 2 :plusinf)
                          '(1 :plusinf :plusinf)
                          '(:minusinf 1 4 :plusinf))
  (assert-not-paired-relation #'xreal:<= #'xreal:>=
                              ;; not < pairs
                              '(2 1)
                              '(:plusinf 1)
                              '(:plusinf :minusinf)
                              '(1 :minusinf)
                              ;; not <=/>= sequences
                              '(1 3 2)
                              '(1 :plusinf 2)))

(deftest equality-test (extended-real-tests)
  (assert-relation #'xreal:=
                   ;; = pairs
                   '(1 1)
                   '(:plusinf :plusinf)
                   '(:minusinf :minusinf)
                   ;; = sequences
                   '(2 2 2)
                   '(:plusinf :plusinf :plusinf)
                   '(:minusinf :minusinf :minusinf))
  (assert-not-relation #'xreal:=
                       ;; not = pairs
                       '(1 2)
                       '(2 1)
                       '(1 :plusinf)
                       '(:plusinf 1)
                       '(1 :minusinf)
                       '(:minusinf 1)
                       ;; not = sequences
                       '(1 2 2)
                       '(2 2 1)
                       '(:plusinf :plusinf 9)
                       '(:plusinf :minusinf)))
