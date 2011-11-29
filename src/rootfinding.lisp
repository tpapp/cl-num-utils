(in-package :cl-num-utils)

;;; Testing convergence of rootfinding methods

(defun opposite-sign? (a b)
  "Return true iff a and b are on opposite sides of 0."
  (or (and (minusp a) (plusp b))
      (and (plusp a) (minusp b))))

(defun narrow-bracket? (a b delta)
  "Return true iff |a-b| < delta."
  (< (abs (- a b)) delta))

(defun near-root? (f epsilon)
  "Return true iff |f| < epsilon."
  (< (abs f) epsilon))

(defparameter *rootfinding-epsilon* (expt double-float-epsilon 0.25)
  "Default EPSILON for rootfinding.")

(defparameter *rootfinding-delta-relative* (expt double-float-epsilon 0.25)
  "Default relative interval width (DELTA) for rootfinding.")

(defun rootfinding-delta (interval
                          &optional (delta-relative *rootfinding-delta-relative*))
  "Default DELTA for rootfinding methods, uses bracket width."
  (* (interval-width interval) delta-relative))

;;; convenience macro for various univariate rootfinders

(defmacro univariate-rootfinder-loop% (((f a b fa fb)
                                        (f-tested test-bracket delta epsilon))
                                       &body body)
  "Common parts for univariate rootfinder functions.

Sets up the following:

- function opposite-sign-p for checking that two numbers are on the
  opposite side of 0

- function evaluate-and-return-if-within-epsilon which checks that
  |f(x)| <= epsilon, if so, returns _from_ toplevel-function-name
  (values x fx t), otherwise simply returns the value

- function return-if-within-tolerance checks if the interval [a,b]
  bracketing x is small enough (smaller than tolerance) and if so,
  returns (x fx nil a b)

- variables fa and fb to hold function values at a and b

Initially, it checks for either f(a) or f(b) being a root, and
establishes a <= b by exchanging a,f(a) and b,f(b) if necessary.  Also
checks that f(a) and f(b) are of opposite sign.  Checks that both
tolerance and epsilon are nonnegative.

Implicitly uses the following variables: f, a, b, tolerance, epsilon."
  (check-types (a b fa fb) symbol)
  (with-unique-names (block-name)
    (once-only (delta epsilon f)
      `(block ,block-name
         (flet ((,f-tested (x)
                  (let ((fx (funcall ,f x)))
                    (if (near-root? fx ,epsilon)
                        (return-from ,block-name (values x fx t ,a ,b))
                        fx)))
                (,test-bracket (fx x)
                  (when (narrow-bracket? ,a ,b ,delta)
                    (return-from ,block-name
                      (values x fx nil ,a ,b)))))
           (assert (and (<= 0 ,delta) (<= 0 ,epsilon)))
           (when (< ,b ,a)
             (rotatef ,a ,b))
           (let* ((,a (coerce ,a 'double-float))
                  (,b (coerce ,b 'double-float))
                  (,fa (,f-tested ,a))
                  (,fb (,f-tested ,b)))
             (unless (opposite-sign? ,fa ,fb)
               (error "Boundaries don't bracket 0."))
             (loop
               ,@body)))))))

(defun root-bisection (f bracket
                       &key (delta (rootfinding-delta bracket))
                            (epsilon *rootfinding-epsilon*))
  "Find the root of f bracketed between a and b using bisection.
The algorithm stops when either the root is bracketed in an interval of length
TOLERANCE (relative to the initial |a-b|), or root is found such that
abs(f(root)) <= epsilon.

Return five values: the root, the value of the function at the root, and a
boolean which is true iff abs(f(root)) <= epsilon.  If the third value is
true, the fourth and fifth values are the endpoints of the bracketing
interval, otherwise they are undefined."
  (let+ (((&interval a b) bracket))
    (univariate-rootfinder-loop% ((f a b fa fb)
                                  (f-tested test-bracket delta epsilon))
      (let* ((m (/ (+ a b) 2))
             (fm (f-tested m)))
        (test-bracket fm m)
        (if (opposite-sign? fa fm)
            (setf b m
                  fb fm)
            (setf a m
                  fa fm))))))

;; (defun root-ridders (f a b &key
;; 		     (tolerance (* (abs (- b a)) #.(expt double-float-epsilon 0.25)))
;; 		     (epsilon #.(expt double-float-epsilon 0.25)))
;;   "Find the root of f bracketed between a and b using Ridders' method.
;; The algorithm stops when either the root is bracketed in an interval
;; of length tolerance, or root is found such that abs(f(root)) <=
;; epsilon.

;; Return five values: the root, the function evaluated at the root, and
;; a boolean which is true iff abs(f(root)) <= epsilon.  If the third
;; value is true, the fourth and fifth values are the endpoints of the
;; bracketing interval, otherwise they are undefined."
;;   ;;   (declare (double-float a b tolerance epsilon)
;;   ;; 	   ((function (double-float) double-float) f))
;;   (univariate-rootfinder-common-setup root-ridders
;;     (macrolet ((new-bracket (a b fa fb)
;; 		 `(progn
;; 		    (setf a ,a
;; 			  b ,b
;; 			  fa ,fa
;; 			  fb ,fb)
;; 		    (go top))))
;;       (tagbody
;;        top
;; ;;;	   (format t "~a ~a~%" a b)
;; 	 (let* ((m (half (+ a b)))			  ; midpoint
;; 		(fm (evaluate-and-return-if-within-epsilon m))) ; value at midpoint
;; 	   (return-if-within-tolerance m fm a b)
;; 	   (let* ((w (- (square fm) (* fa fb)))	; discriminant
;; 		  (delta (/ (* (signum fa) fm (- b m)) (sqrt w))) ; c-m
;; 		  (c (+ m delta))	; interpolated guess
;; 		  (fc (evaluate-and-return-if-within-epsilon c))) ; value at guess
;; 	     (if (minusp delta)
;; 		 ;; c < m
;; 		 (cond
;; 		   ((opposite-sign-p fm fc) (new-bracket c m fc fm))
;; 		   ((opposite-sign-p fa fc) (new-bracket a c fa fc))
;; 		   ((opposite-sign-p fb fc) (new-bracket c b fc fb))
;; 		   (t (error "internal error")))
;; 		 ;; m < c
;; 		 (cond
;; 		   ((opposite-sign-p fm fc) (new-bracket m c fm fc))
;; 		   ((opposite-sign-p fb fc) (new-bracket c b fc fb))
;; 		   ((opposite-sign-p fa fc) (new-bracket a c fa fc))
;; 		   (t (error "internal error"))))))))))



;; (defun find-satisfactory-fx (x f next-x-rule &key
;; 			     (satisfactory-p #'minusp)
;; 			     (maximum-iterations 100))
;;   "Try a sequence of x's (starting from x, generating the next one by
;; next-x-rule) until f(x) satisfies the predicate.  Return (values x
;; fx).  If maximum-iterations are reached, an error is signalled."
;;   (dotimes (i maximum-iterations)
;;     (let ((fx (funcall f x)))
;;       (if (funcall satisfactory-p fx)
;; 	(return-from find-satisfactory-fx (values x fx))
;; 	(setf x (funcall next-x-rule x)))))
;;   ;; !!!! todo: decent error reporting with a condition
;;   (error "reached maximum number of iterations"))


;; (defun make-expanding-rule (deltax multiplier)
;;   "Creates a function that adds an ever-increasing (starting with
;; deltax, multiplied by multiplier at each step) to its argument.
;; Primarily for use with root-autobracket."
;;   (assert (< 1 multiplier))
;;   (lambda (x)
;;     (let ((new-x (+ x deltax)))
;;       (multf deltax multiplier)
;;       new-x)))

;; (defun make-contracting-rule (attractor coefficient)
;;   "Creates a function that brings its argument closer to attractor,
;; contracting the distance by coefficient at each step.  Primarily for
;; use with autobracket."
;;   (assert (< 0 coefficient 1))
;;   (lambda (x)
;;     (+ (* (- x attractor) coefficient) attractor)))
      
;; (defun root-autobracket (f x negative-rule positive-rule
;; 			 &key (maximum-iterations 100)
;; 			 (rootfinder #'root-ridders)
;; 			 (tolerance #.(expt double-float-epsilon 0.25))
;; 			 (epsilon #.(expt double-float-epsilon 0.25)))
;;   "Rootfinder with automatic bracketing.  First we evaluate at x, and
;; check if it is a root.  If not, and f(x) is positive, we try to locate
;; a satisfactory bracket by generating x's using positive-rule.  Mutatis
;; mutandis if f(x) is negative.

;; Since the bracket is not known beforehand, you can only specify a
;; relative tolerance.  For the meaning of other parameters, see
;; rootfinding functions and find-satisfactory-fx."
;;   (assert (<= 0 epsilon))
;;   (let ((fx (funcall f x)))
;;     (cond
;;       ;; found root
;;       ((<= (abs fx) epsilon)
;;        (values x fx))
;;       ;; no root, trying to find a negative value for bracketing
;;       ((plusp fx)
;;        (bind (((values y fy) (find-satisfactory-fx x f positive-rule
;; 			      :satisfactory-p #'minusp
;; 			      :maximum-iterations maximum-iterations)))
;; 	 (if (<= (abs fy) epsilon)
;; 	     (values y fy)
;; 	     (funcall rootfinder f x y
;; 		      :tolerance ;(* (absolute-difference x y)
;; 				    tolerance ;)
;; 		      :epsilon epsilon))))
;;       ((minusp fx)
;;        (bind (((values y fy) (find-satisfactory-fx x f negative-rule
;; 			      :satisfactory-p #'plusp
;; 			      :maximum-iterations maximum-iterations)))
;; 	 (if (<= (abs fy) epsilon)
;; 	     (values y fy)
;; 	     (funcall rootfinder f x y
;; 		      :tolerance ;(* (absolute-difference x y)
;; 				    tolerance ;)
;; 		      :epsilon epsilon)))))))

;;; (root-autobracket #'identity 5 (make-expanding-rule 1 2)
;;;		  (make-expanding-rule -1 2))
