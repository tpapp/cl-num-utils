;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-num-utils)

(defun golden-section-combination (a b)
  "Return the convex combination (1-G)*a+G*b, where G is the
inverse of the golden ratio."
  (+ (* #.(- 1d0 (/ (- 3d0 (sqrt 5d0)) 2d0)) a)
     (* #.(/ (- 3d0 (sqrt 5d0)) 2d0) b)))

(defun golden-section-minimize (f a b tol &optional (max-iter 100))
  "Find a local minimum of F in the [A,B] interval.  The algorithm terminates
when the minimum is bracketed in an interval smaller than TOL.  Since the
algorithm is slow, TOL should not be chosen smaller then necessary.  The
algorithm will also find the local minimum at the endpoints, and if F is
unimodal, it will find the global minimum.  MAX-ITER is there for terminating
the algorithm, in case tolerance is zero or too small.  All values (except
max-iter) should be double-float, and F should be of
type (FUNCTION (DOUBLE-FLOAT) DOUBLE-FLOAT).

Note: when F is constant on a range, golden-section-minimize ``pulls
to the left'', ie will keep picking smaller values."
  (declare (double-float a b tol)
	   (fixnum max-iter)
	   (type (function (double-float) double-float) f)
	   (inline golden-section-combination)
	   (optimize speed (safety 1)))
  ;; reorder a and b if necessary
  (when (> a b)
    (rotatef a b))
  ;; start iteration with golden ratio inner points
  (let* ((m1 (golden-section-combination a b))
	 (m2 (golden-section-combination b a))
	 (f1 (funcall f m1))
	 (f2 (funcall f m2)))
    (declare (double-float m1 m2 f1 f2))
    (iter
      (repeat max-iter)
      (declare (iterate:declare-variables))
      (when (<= (abs (- b a)) tol)
        (return-from golden-section-minimize
          (if (< f1 f2)			; change < to maximize
              (values m1 f1)
              (values m2 f2))))
      (if (<= f1 f2)			; change <= to maximize
	  (progn
	    ;; new bracket is (a,m1,m2)
	    (shiftf b m2 m1 (golden-section-combination m1 a))
	    (shiftf f2 f1 (funcall f m1)))
	  (progn
	    ;; new bracket is (m1,m2,b)
	    (shiftf a m1 m2 (golden-section-combination m2 b))
	    (shiftf f1 f2 (funcall f m2)))))
    (error 'reached-maximum-iterations :n max-iter)))

;; (defun linesearch-backtrack (g g0 gp0 alpha delta  &key
;;                              (rel-min 0.1d0) (rel-max 0.5d0) (c 1d-4)
;;                              (max-iter 100))
;;   "Find alpha such that g(alpha) <= g(0) + c g'(0) alpha.

;; Parameters: G: the function g, G0: g(0), GP0: g'(0), ALPHA: initial alpha,
;; usually 1, for quasi-Newton methods, DELTA is the threshold for being too close
;; to 0 (perhaps indicating convergence). C is as above.  Uses the backtracking
;; method."
;;   (check-types double-float g0 gp0 alpha delta rel-min rel-max c)
;;   (assert (plusp alpha))
;;   (assert (< 0d0 delta alpha))
;;   (assert (plusp c) () "C should be positive.")
;;   (assert (minusp g0) () "Nonnegative g'(0).")
;;   (let (alpha-prev
;;         g-alpha-prev
;;         (slope (* gp0 c)))              ; line for sufficient decrease
;;     (iter
;;       (repeat max-iter)
;;       (let ((g-alpha (funcall g alpha)))
;;         ;; found satisfactory value
;;         (when (<= g-alpha (+ g0 (* slope alpha)))
;;           (return-from linesearch-backtrack alpha))
;;         ;; below delta, possible convergence
;;         (when (<= alpha delta)
;;           (return-from linesearch-backtrack 0))
;;         ;; calculate next step
;;         (let* ((alpha-next
;;                 (if alpha-prev
;;                     ;; cubic approximation
;;                     (let* ((r (- g-alpha (* gp0 alpha) g0))
;;                            (r-prev (- g-alpha-prev (* gp0 alpha-prev) g0))
;;                            (alpha-diff (- alpha alpha-prev))
;;                            (s (expt alpha 2))
;;                            (s-prev (expt alpha-prev 2))
;;                            (a (/ (- (/ r s) (/ r-prev s-prev)) alpha-diff))
;;                            (b (/ (- (/ (* alpha r-prev) s-prev)
;;                                     (/ (* alpha-prev r) s))
;;                                  alpha-diff)))
;;                       (if (zerop a)
;;                           (- (/ gp0 b 2d0))
;;                           (let ((discriminant (- (expt b 2d0) (* 3 a gp0))))
;;                             (cond
;;                               ;; a guess, will be regularized anyway
;;                               ((minusp discriminant) alpha)
;;                               ;; positive b: take left root
;;                               ((plusp b) (/ (- gp0) (+ b (sqrt discriminant))))
;;                               ;; negative b: take right root
;;                               (t (/ (- (sqrt discriminant) b) a 3d0))))))
;;                     ;; quadratic approximation
;;                     (- (/ (* gp0 (square alpha))
;;                           (- g-alpha g0 (* alpha gp0)) 2d0)))))
;;           (setf alpha-prev alpha
;;                 g-alpha-prev g-alpha
;;                 alpha (min (max (* alpha rel-min)
;;                                 alpha-next)
;;                            (* alpha rel-max))))))
;;     (error 'reached-max-iter)))
