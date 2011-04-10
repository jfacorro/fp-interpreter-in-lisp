(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Split by " "
;;--------------------------------------
(add-rule (defrule 
			"Split by space"
			(lambda (str) (string-split str #\ ))))
;:--------------------------------------
;; Explode by (
;;--------------------------------------
(add-rule (defrule 
			"Explode by ("
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\( )) lst)))))
;:--------------------------------------
;; Explode by )
;;--------------------------------------
(add-rule (defrule 
			"Explode by )"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\) )) lst)))))
;:--------------------------------------
;; Explode by ;
;;--------------------------------------
(add-rule (defrule 
			"Explode by ;"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\; )) lst)))))
;:--------------------------------------
;; Explode by ~
;;--------------------------------------
(add-rule (defrule 
			"Explode by ~"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\~ )) lst)))))
;:--------------------------------------
;; Explode by [
;;--------------------------------------
(add-rule (defrule 
			"Explode by ["
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\[ )) lst)))))
;:--------------------------------------
;; Explode by ]
;;--------------------------------------
(add-rule (defrule 
			"Explode by ]"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el #\] )) lst)))))
;;----------------------------------------------
;; Flatten
;;----------------------------------------------
(defun flatten (l)
	(cond
		((null l) nil)
		((atom l) (list l))
		(t (append 	(flatten (car l))
					(flatten (cdr l))))))
;;----------------------------------------------
(apply-all-rules "/ appendlr <> o (alpha (atom -> id; ~<>))")