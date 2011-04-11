(defpackage :com.facorro.string
	(:use 
		:common-lisp)
	(:export 
		:string-split
		:string-explode
		:flatten))
(defpackage :com.facorro.parser
	(:use 
		:common-lisp)
	(:export 
		:*rules*
		:add-rule
		:parse
		:apply-rule
		:defrule))
(defpackage :com.facorro.fp.parser
	(:use 
		:common-lisp
		:com.facorro.parser
		:com.facorro.string))
(defpackage :com.facorro.fp
	(:use :common-lisp))
(defpackage :com.facorro.fp.functions
	(:use :common-lisp)
	(:export 
		:get-function
		:*functions*))
(defpackage :com.facorro.test
	(:use :common-lisp)
	(:export :test))
(defpackage :com.facorro.fp.interpreter
	(:use 
		:common-lisp 
		:com.facorro.fp.functions
		:com.facorro.parser
		:com.facorro.test))