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
(defun build-tree-helper (lst tokens root)
	(format t "build-tree-helper code: ~a~% tokens: ~a~% root: ~a~%" lst tokens root)
	(let* ((token (first lst))
		   (fun (get-function token))
		   (lst-tail (rest lst)))
		(format t "token: \"~a\"~%" token)
		(if (null lst)
			root
			(cond 
				; No fp-function => it's a special symbol
				((stringp fun)
					(cond 
						((string= fun "(") (build-tree-helper lst-tail nil root))
						((string= fun ")") root)
						((string= fun ";") (build-tree-helper lst-tail nil root))
						((string= fun "<>") (add-child root (make-node token)))))
				; No paramaters => add to tokens list
				((noparams-p fun)
					(if (not (null root))
						(add-child root (make-node token))
						(setf tokens (cons token tokens)))
					(build-tree-helper lst-tail tokens root))
				(t 
					(let ((newroot (make-node token)) 
						  (rootname (datum root)))
						(if (not (null root))
							(if (most-precedence token rootname)
								(add-child newroot root)
								(add-child root newroot)))
						(if (not (null tokens))
							(dolist (item tokens) (add-child newroot (make-node item))))
						(build-tree-helper lst-tail nil newroot)))
			))))
;;----------------------------------------------
;; process-special-symbol
;;----------------------------------------------
(defun process-special-symbol (lst token)
	nil)