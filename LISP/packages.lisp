(defpackage :com.facorro.lisp.fp
	(:use :common-lisp))
(defpackage :com.facorro.lisp.fp.functions
	(:use :common-lisp))
(defpackage :com.facorro.test
	(:use :common-lisp))
(defpackage :com.facorro.lisp.fp.interpreter
	(:use :common-lisp :com.facorro.lisp.fp.functions :com.facorro.test))