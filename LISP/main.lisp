;(defparameter *filepath* "C:\\Juan\\02.Personal\\Dropbox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP\\")
(defparameter *filepath* "C:\\JF\\My DropBox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP\\")
;----------------------------------------------------
(defun compile-load (file-name &optional (v nil) (p nil))
		(load (compile-file (concatenate 'string *filepath* file-name) :verbose v :print p) :verbose v :print p))
;----------------------------------------------------
(progn
	(compile-load "packages.lisp")
	(compile-load "debug.lisp")
	(compile-load "tree.lisp")
	(compile-load "string.lisp")	
	(compile-load "parser.lisp")
	(compile-load "fp-parser.lisp")
	(compile-load "fp-functions-definition.lisp")
	(compile-load "fp-functions.lisp")
	(compile-load "test-framework.lisp")
	;; (load-code "tests.lisp")
	(compile-load "fp-interpreter.lisp" nil nil)
	(compile-load "fp-evaluator.lisp")
	(compile-load "fp-repl.lisp" nil nil))

(in-package :com.facorro.string)
(string-explode "11.11.11.1.1111,111" "." ",")
(string-split "11.11.11.1.1111,111" "." ",")
#|
(in-package :com.facorro.debug)
(debug-on)
(in-package :com.facorro.fp.interpreter)
(funcall (interpret "id") '1)
(in-package :com.facorro.fp.repl)
(fp-repl)
|#