;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils-tests)

(deftestsuite ix-tests (cl-num-utils-tests)
  ()
  (:equality-test #'equalp))

(addtest (ix-tests)
  simple-ix-tests
  (bind ((specification '((alpha . 3) (beta) (gamma . 4)))
         (ix (make-ix specification))
         (numbers (iseq (ix-size ix)))
         ((:flet numbers (&rest index)) (sub numbers (apply #'ix ix index))))
    (ensure-same (numbers 'alpha) #(0 1 2))
    (ensure-same (numbers 'beta) 3)
    (ensure-same (numbers 'gamma) #(4 5 6 7))
    (ensure-same (numbers 'gamma 2) 6)
    (ensure-same (numbers) numbers)
    (ensure-error (numbers 9))          ; outside range
    (ensure-error (numbers 'kappa))     ; invalid key
    (ensure-error (numbers 0 0 0))      ; too many keys
    (ensure-error (numbers 'alpha 7))   ; invalid key
    (ensure-same (ix->specification ix) specification)))

(addtest (ix-tests)
  complex-ix-tests
  (bind ((specification '((alpha (beta) (gamma . 4) (epsilon 2)) (delta . #(2 3))))
         (ix (make-ix specification))
         (numbers (iseq (ix-size ix)))
         ((:flet numbers (&rest index)) (sub numbers (apply #'ix ix index))))
    (ensure-same (numbers 'alpha) #(0 1 2 3 4 5 6))
    (ensure-same (numbers 'alpha 'beta) 0)
    (ensure-same (numbers 'alpha 'gamma) #(1 2 3 4))
    (ensure-same (numbers 'alpha 'gamma 2) 3)
    (ensure-same (numbers 'alpha 'epsilon) #(5 6))
    (ensure-same (numbers 'alpha 'epsilon 0) #(5 6))
    (ensure-same (numbers 'alpha 'epsilon 0 1) 6)
    (ensure-same (numbers 'delta) #(7 8 9 10 11 12))
    (ensure-same (numbers 'delta 0) #(7 8 9))
    (ensure-same (numbers 'delta 1) #(10 11 12))
    (ensure-error (numbers 'delta 2))
    (ensure-same (numbers 'delta 0 1) 8)
    (ensure-same (numbers 'delta 1 2) 12)
    (ensure-error (numbers 'delta 0 4))
    (ensure-error (numbers 'delta 0 0 0))))

(addtest (ix-tests)
  sub-ix-tests
  (bind ((specification '((alpha . 3) (beta) (gamma (delta . 1) (kappa . 3))))
         (ix (make-ix specification)))
    (ensure-same (sub ix 'alpha) 3)
    (ensure-same (ix->specification (sub ix #(alpha beta))) (sub specification #(0 1)))
    (ensure-same (ix->specification (sub ix (si 1 0))) (sub specification #(1 2)))
    (ensure-error (sub ix 0 1))         ; too many arguments
    (ensure-error (sub ix 'rho))))      ; invalid key
