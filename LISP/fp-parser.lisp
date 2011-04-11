(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Split by " "
;;--------------------------------------
(add-rule (defrule 
			"Split by space"
			(lambda (str) (string-split str " "))))

;:--------------------------------------
;; Explode by (
;;--------------------------------------
(add-rule (defrule 
			"Explode by ("
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "(" ")" ";" "/" "->" "[" "]" "~")) lst)))))
#|
;:--------------------------------------
;; Explode by )
;;--------------------------------------
(add-rule (defrule 
			"Explode by )"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el ")")) lst)))))
;:--------------------------------------
;; Explode by ;
;;--------------------------------------
(add-rule (defrule 
			"Explode by ;"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el ";")) lst)))))
;:--------------------------------------
;; Explode by ~
;;--------------------------------------
(add-rule (defrule 
			"Explode by ~"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "~")) lst)))))
;:--------------------------------------
;; Explode by [
;;--------------------------------------
(add-rule (defrule 
			"Explode by ["
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "[" )) lst)))))
;:--------------------------------------
;; Explode by ]
;;--------------------------------------
(add-rule (defrule 
			"Explode by ]"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "]")) lst)))))
;:--------------------------------------
;; Explode by ->
;;--------------------------------------
(add-rule (defrule 
			"Explode by ->"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "->")) lst)))))
;:--------------------------------------
;; Explode by /
;;--------------------------------------
(add-rule (defrule 
			"Explode by /"
			(lambda (lst) 
				(flatten 
					(mapcar (lambda (el) (string-explode el "/")) lst)))))
|#
;;----------------------------------------------
(parse "/appendr o(alpha(atom->id;~<>))")