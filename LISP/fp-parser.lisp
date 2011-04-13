(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Convert to uppercase
;;--------------------------------------
(add-rule (defrule
			"Convert to uppercase"
			(lambda (str) (string-upcase str))))
;:--------------------------------------
;; Replace some expression for parser-friendly
;; new expressions
;;--------------------------------------
(add-rule (defrule
			"Replace for parser-friendly expressions"
			(lambda (str) 
				(string-replace str 
					"[" "(construct(("
					"]" "))"
					"," ")("))))
;:--------------------------------------
;; Split by " " ;
;;--------------------------------------
(add-rule (defrule
			"Split by space"
			(lambda (str) (string-split str " " ";"))))
;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule (defrule 
			"Explode by ( ) ; / -> [ ] ~ + - % * º ºr"
			(lambda (lst) 
				(string-explode lst "(" ")" ";" "/" "=>" "[" "]" "~" "+" "-" "%" "*" "º" "ºr"))))
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
#|
(add-rule (defrule 
			"Build tree"
			;(lambda (lst) lst)))			
			(lambda (lst) (build-tree lst))))
|#
;:--------------------------------------
;; listify
;;--------------------------------------
(defun listify (expr &optional (parent nil) (child nil))
	(debug-msg "(listify)~%")
	(debug-msg "  expr: ~a~%" expr)
	(debug-msg "  parent: ~a~%" parent)
	(debug-msg "  child: ~a~%" child)

	(if (atom expr)	expr
		(let ((head (first expr))
			  (tail (rest expr)))
			(cond
				((string= head "(") (append parent child (list (listify tail))))
				((string= head ")") 
					(append parent 
						(if (null child) nil (list child))
						(if (null tail) nil (listify tail))))
				(t 
					(listify tail parent (append child (list head))))))))
			
;(listify '("1" "+" "2" "(" "1" "/" "(" "4" "+" "8" ")" ")"))
;0 ( 1 ( 2 ) 3 ) 4
;;----------------------------------------------
;; build-tree
;;----------------------------------------------
(defun build-tree (lst)
	(debug-msg "build-tree \"~a\"~%" lst)
	(build-tree-helper lst nil nil))
;;----------------------------------------------
;; build-tree-helper
;;----------------------------------------------
(defun build-tree-helper (code operators operands)
	(debug-msg "build-tree-helper~%")
	(debug-msg "  code: ~a~%" code)
	(debug-msg "  operators: ~a~%" operators)
	(debug-msg "  operands: ~a~%" operands)

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
				(setf operands (cons (make-node token) operands))
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
	(debug-msg "handle-operator~%")
	(debug-msg "  code: ~a~%" code)
	(debug-msg "  operators: ~a~%" operators)
	(debug-msg "  operands: ~a~%" operands)
	
	(let* ((operator (first operators))
		   (fn (get-function operator))
		   (nparams (num-params fn)))		
		(build-tree-helper
			code
			(rest operators)
			(cons 
				(apply #'make-node (append (list operator) (reverse (subseq operands 0 nparams))))
				(subseq operands nparams)))))