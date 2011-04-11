(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Split by " "
;;--------------------------------------
(add-rule (defrule 
			"Split by space"
			(lambda (str) (string-split str " "))))

;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule (defrule 
			"Explode by ("
			(lambda (lst) 
				(string-explode lst "(" ")" ";" "/" "->" "[" "]" "~"))))
;;----------------------------------------------
(parse "/appendr o(alpha(atom->id;~<>))")