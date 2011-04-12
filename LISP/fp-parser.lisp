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
;; Convert parenthesis in sublists
;;--------------------------------------
(add-rule (defrule 
			"Convert parenthesis in sublists"
			(lambda (lst) 
				(listify lst))))
;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule (defrule 
			"Build tree"
			;(lambda (lst) lst)))			
			(lambda (lst) (build-tree lst))))
;:--------------------------------------
;; listify
;;--------------------------------------
(defun listify (expr)
	(if (atom expr)	expr
		(let ((head (first expr))
			  (tail (rest expr)))
			(cond
				((string= head "(") (list (listify tail)))
				((string= head ")") nil)
				(t (cons head (listify tail)))))))
;(listify '("1" "+" "2" "(" "1" "/" "(" "4" "+" "8" ")" ")"))
;;----------------------------------------------
;; build-tree
;;----------------------------------------------
(defun build-tree (lst)
	(format t "build-tree \"~a\"~%" lst)
	(build-tree-helper lst nil nil))
;;----------------------------------------------
;; build-tree-helper
;;----------------------------------------------
(defun build-tree-helper (code operators operands)
	(format t "build-tree-helper~%")
	(format t "  code: ~a~%" code)
	(format t "  operators: ~a~%" operators)
	(format t "  operands: ~a~%" operands)

	(let* ((token (first code))		   
		   (fn (get-function token)))
		;(format t "  token: \"~a\"~%" token)
		(cond 
			((null code)
				(if (null operators)
					(car operands)
					(handle-operator nil operators operands)))
			((listp token) 
					(let ((subexpr (build-tree-helper token nil nil)))
						(setf operands (cons subexpr operands))
						(build-tree-helper (rest code) operators operands)))
			((operand? fn)
				(if (not (string= token ";")) (setf operands (cons (make-node token) operands)))
				(build-tree-helper (rest code) operators operands))
			(t
				(let* ((last-op (first operators))
					   (last-fn (get-function last-op)))
					(if (or (null operators)
							(> (precedence fn) (precedence last-fn)))
						(build-tree-helper (rest code) (cons token operators) operands)
						(handle-operator code operators operands)))))))
;;----------------------------------------------
;; handle-operator
;;----------------------------------------------
(defun handle-operator (code operators operands)
	(format t "handle-operator~%")
	(format t "  code: ~a~%" code)
	(format t "  operators: ~a~%" operators)
	(format t "  operands: ~a~%" operands)
	
	(let* ((operator (first operators))
		   (fn (get-function operator))
		   (nparams (num-params fn)))		
		(build-tree-helper
			code
			(rest operators)
			(cons 
				(apply #'make-node (append (list operator) (reverse (subseq operands 0 nparams))))
				(subseq operands nparams)))))