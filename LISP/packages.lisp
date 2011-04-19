;----------------------------------------------------
(defpackage :com.facorro.debug
	(:use 
		:common-lisp)
	(:export
		:*debugging-package?*
		:debugging?
		:debug-on
		:debug-off
		:debug-msg))
;----------------------------------------------------
(defpackage :com.facorro.tree
	(:use 
		:common-lisp
		:com.facorro.debug)
	(:export
		:make-node
		:children
		:datum
		:add-child))
;----------------------------------------------------
(defpackage :com.facorro.string
	(:use 
		:common-lisp
		:com.facorro.debug)
	(:export 
		:string-split
		:string-explode
		:string-explode-sequentially
		:string-replace
		:empty-string?
		:contains?
		:flatten
		:insert-between))
;----------------------------------------------------
(defpackage :com.facorro.parser
	(:use 
		:common-lisp
		:com.facorro.debug)
	(:export 
		:*rules*
		:add-rule
		:parse
		:apply-rule
		:make-rule))
;----------------------------------------------------
(defpackage :com.facorro.fp.functions
	(:use 
		:common-lisp
		:com.facorro.debug)
	(:export 
		:get-function
		:reset-functions
		:operand?
		:precedence
		:num-params
		:resolve-operand
		:*false-value*
		:*true-value*
		:*empty-list-value*
		:*functions*))
;----------------------------------------------------
(defpackage :com.facorro.fp.parser
	(:use 
		:common-lisp
		:com.facorro.parser
		:com.facorro.string
		:com.facorro.fp.functions
		:com.facorro.tree
		:com.facorro.debug))
;----------------------------------------------------
(defpackage :com.facorro.fp.evaluator
	(:use 
		:common-lisp
		:com.facorro.debug
		:com.facorro.fp.functions
		:com.facorro.tree
		:com.facorro.string)
	(:export
		:evaluate
		:evaluate-env
		:desymbolize))
;----------------------------------------------------
(defpackage :com.facorro.test
	(:use :common-lisp)
	(:export :test))
;----------------------------------------------------
(defpackage :com.facorro.fp.interpreter
	(:use 
		:common-lisp
		:com.facorro.debug
		:com.facorro.fp.functions
		:com.facorro.parser
		:com.facorro.test
		:com.facorro.fp.evaluator)
	(:export
		:interpret))
;----------------------------------------------------
(defpackage :com.facorro.fp.repl
	(:use
		:common-lisp
		:com.facorro.fp.interpreter
		:com.facorro.fp.functions
		:com.facorro.string
		:com.facorro.debug)
	(:export
		:fp-repl))
;----------------------------------------------------