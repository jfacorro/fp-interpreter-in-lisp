(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Replace some expression for parser-friendly
;; new expressions
;;--------------------------------------
(add-rule (defrule
			"Replace for parser-friendly expressions"
			(lambda (str) 
				(string-replace str 
					"[" "( construct ("
					"]" "))"
					"," ")("))))
;:--------------------------------------
;; Convert to uppercase
;;--------------------------------------
(add-rule (defrule
			"Convert to uppercase"
			(lambda (str) (string-upcase str))))

;:--------------------------------------
;; Split by " " ;
;;--------------------------------------
(add-rule (defrule
			"Split by space ;"
			(lambda (str) (string-split str " " ";"))))
;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule (defrule 
			"Explode by ( ) / -> [ ] ~ + - % * � �r"
			(lambda (lst) 
				(string-explode lst "(" ")" "/" "=>" "[" "]" "~" "+" "-" "%" "*" "�" "�r"))))
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
(defun listify (expr &optional (lists nil))
	(debug-msg "(listify)~%")
	(debug-msg "  expr: ~a~%" expr)
	(debug-msg "  lists: ~a~%" lists)

	(if (atom expr)	(first lists)
		(let* ((head (first expr))
			   (tail (rest expr))
			   (lists (if (null lists) (list nil) lists))
			   (current (first lists))
			   (next (second lists))
			   (else (cddr lists)))
			(debug-msg "  head: ~a~%" head)
			(debug-msg "  current: ~a~%" current)
			(debug-msg "  next: ~a~%" next)
			(debug-msg "  else: ~a~%" else)
			(cond
				((string= head "(") 
					(listify tail (cons nil lists)))
				((string= head ")")
					(listify tail (append (list (append next (list current))) else)))
				(t
					(listify tail (append (list (append current `(,head))) (list next) else)))))))
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
		; If the number of parameters specified for the function
		; is a negative number then take all available operands
		(if (< nparams 0) (setf nparams (length operands)))
		(build-tree-helper
			code
			(rest operators)
			(cons 
				(apply #'make-node (cons operator (reverse (subseq operands 0 nparams))))
				(subseq operands nparams)))))
		