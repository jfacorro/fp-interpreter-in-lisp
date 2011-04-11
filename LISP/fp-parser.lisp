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
			"Explode by ( ) ; / -> [ ] ~"
			(lambda (lst) 
				(string-explode lst "(" ")" ";" "/" "->" "[" "]" "~"))))
;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule (defrule 
			"Build tree"
			;(lambda (lst) lst)))			
			(lambda (lst) (build-tree lst))))
;;----------------------------------------------
;; build-tree
;;----------------------------------------------
(defun build-tree (lst)
	(format t "build-tree \"~a\"~%" lst)
	(build-tree-helper lst nil nil))
;;----------------------------------------------
;; build-tree-helper
;;----------------------------------------------
(defun build-tree-helper (lst tokens tree)
	(format t "build-tree-helper \"~a\"~% ~a~%" lst tokens)
	(let* ((token (first lst))
		   (fp-fun (get-function token))
		   (lst-tail (rest lst)))
		(if (null lst) 
			tree
			(cond 
				; No fp-function => it's a special symbol
				((null fp-fun) (process-special-symbol lst-tail token))
				; No paramaters => add to tokens list
				((noparams-p fp-fun) (build-tree-helper lst-tail (cons token tokens) tree))
				(t 
					(let ((node (make-tree-node token)))
						(build-tree-helper lst-tail tokens node)))
			))))
;;----------------------------------------------
;; process-special-symbol
;;----------------------------------------------
(defun process-special-symbol (lst token)
	nil)