(defpackage :com.facorro.lisp.fp
	(:use :common-lisp))
(defpackage :com.facorro.lisp.fp.functions
	(:use :common-lisp)
	(:export 
		:get-function
		:*functions*
		:id))
(defpackage :com.facorro.test
	(:use :common-lisp)
	(:export :test))
(defpackage :com.facorro.lisp.fp.interpreter
	(:use 
		:common-lisp 
		:com.facorro.lisp.fp.functions
		:com.facorro.test))