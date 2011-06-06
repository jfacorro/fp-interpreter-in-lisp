(in-package :com.facorro.fp.parser)
;:--------------------------------------
;; Checks that the expression contains a :
;; which means it is either a user function
;; definition or a function call
;;--------------------------------------
(add-rule 
	(make-rule "Validate expression"
		(cond
			((comment? arg) nil)
			(t 
				(validate-expression arg)
				(generate-fn-env arg)))))
;:--------------------------------------
;; validate-expression
;;--------------------------------------
(defun validate-expression (arg)
	(unless (or (evaluation? arg) (definition? arg))
		(error "The expression must be a function definition or a function evaluation." )))
;:--------------------------------------
;; definition?
;;--------------------------------------
(defun definition? (arg)
	(let* ((arg (string-trim " " arg))
			(index-def (search "def " arg)))
		(and (contains? "=" arg) 
			(zerop index-def)
			(= 1 (count #\= arg)))))
;:--------------------------------------
;; evaluation?
;;--------------------------------------
(defun evaluation? (arg)
	(and (contains? ":" arg) 
		(not (definition? arg))
		(= 1 (count #\: arg))))
;:--------------------------------------
;; Replace some expression for parser-friendly
;; new expressions
;;--------------------------------------
(add-rule 
	(make-rule "Replace strings for parser-friendly expressions"
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))
			
			(list :fn (string-replace fn 
										; construct
										"[" "( construct ("
										"]" "))"
										"," ")("
										; condition
										"->" "->("
										";" ")")
			:env (string-replace env "<" "("
										">" ")")))))
;:--------------------------------------
;; Convert to uppercase
;;--------------------------------------
(add-rule 
	(make-rule "Convert to uppercase" 
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))

			(list :fn (string-upcase fn) :env (string-upcase env)))))
;:--------------------------------------
;; Split by " " ;
;;--------------------------------------
(add-rule 
	(make-rule "Split by ' ' and ';'" 
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))

			(list :fn (string-split fn ":" " " ";" "=") :env (string-split env "," " ") ))))
;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule 
	(make-rule "Explode by special characters"
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))

			(list :fn (string-explode-sequentially fn "(" ")" "&" "/" "->" "<>" ">" "<" "[" "]" "~" "+" "-" "%" "*" "#R" "#")
			  :env (string-explode env "(" ")")))))
;:--------------------------------------
;; Convert parenthesis in sublists
;;--------------------------------------
(add-rule 
	(make-rule "Convert parenthesis in sublists" 
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))

			(list :fn (listify fn) :env (listify env)))))
;:--------------------------------------
;; Build tree
;;--------------------------------------
(add-rule 
	(make-rule "Build tree" 
		(let ((fn (getf arg :fn))
			  (env (getf arg :env)))

		(list :fn (build-tree fn) :env env))))
;:--------------------------------------
;; listify
;;--------------------------------------
(defun listify (expr &optional (lists nil))
	(debug-msg :com.facorro.fp.parser "(listify)~%")
	(debug-msg :com.facorro.fp.parser "  expr: ~a~%" expr)
	(debug-msg :com.facorro.fp.parser "  lists: ~a~%" lists)
	(if (atom expr)	(first lists)
		(let* ((head (first expr))
			   (tail (rest expr))
			   (current (first lists))
			   (next (second lists))
			   (else (cddr lists)))
			(debug-msg :com.facorro.fp.parser "  head: ~a~%" head)
			(debug-msg :com.facorro.fp.parser "  current: ~a~%" current)
			(debug-msg :com.facorro.fp.parser "  next: ~a~%" next)
			(debug-msg :com.facorro.fp.parser "  else: ~a~%" else)
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
	(debug-msg :com.facorro.fp.parser "build-tree \"~a\"~%" lst)
	(build-tree-helper lst nil nil))
;;----------------------------------------------
;; build-tree-helper
;;----------------------------------------------
(defun build-tree-helper (code operators operands)
	(debug-msg :com.facorro.fp.parser "build-tree-helper~%")
	(debug-msg :com.facorro.fp.parser "  code: ~a~%" code)
	(debug-msg :com.facorro.fp.parser "  operators: ~a~%" operators)
	(debug-msg :com.facorro.fp.parser "  operands: ~a~%" operands)
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
	(debug-msg :com.facorro.fp.parser "handle-operator~%")
	(debug-msg :com.facorro.fp.parser "  code: ~a~%" code)
	(debug-msg :com.facorro.fp.parser "  operators: ~a~%" operators)
	(debug-msg :com.facorro.fp.parser "  operands: ~a~%" operands)
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
;;----------------------------------------------
;; generate-fn-env
;;----------------------------------------------
(defun generate-fn-env (arg)
	"Generates an alist with :fn and :env"
	(let* ((parts (string-split arg ":"))
			(fn (first parts))
			(env (second parts)))
		; If it's a function definition don't assign any env
		(if (definition? arg)
			; Function definition
			(list :fn arg :env nil)
			; Function (or expression) evaluation
			(list :fn fn :env env))))
;;----------------------------------------------
;; comment?
;;----------------------------------------------
(defun comment? (arg)
	"Returns true if the arg starts with a ;"
	(let ((index (search ";" arg)))
		(and (not (null index)) (zerop index))))