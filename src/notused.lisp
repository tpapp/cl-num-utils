;; (defun pick-keyword-pairs (list)
;;   "Return keywords followed by items as a flat list, and the rest of
;; the list as the second value."
;;   (iter
;;     (while list)
;;     (bind (((first . rest) list))
;;       (setf list
;;             (if (keywordp first)
;;                 (progn 
;;                   (assert rest () "List ends after keyword.")
;;                   (collect first :into pairs)
;;                   (collect (car rest) :into pairs)
;;                   (cdr rest))
;;                 (progn
;;                   (collect first :into new-list)
;;                   rest))))
;;     (finally
;;      (return (values pairs new-list)))))

;; (pick-keyword-pairs '(:foo bar baz 1 2 3 :last 4))
