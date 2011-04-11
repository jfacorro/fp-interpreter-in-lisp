(defparameter *filepath* "C:\\Juan\\02.Personal\\Dropbox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP\\")
;(defparameter *filepath* "C:\\JF\\My DropBox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP\\")

(defun load-code (file-name &optional (v nil) (p nil))
		(load (concatenate 'string *filepath* file-name) :verbose v :print p))

(progn
	(load-code "packages.lisp")
	(load-code "tree.lisp")
	(load-code "string.lisp")	
	(load-code "parser.lisp")
	(load-code "fp-parser.lisp")
	(load-code "fp-functions-definition.lisp")
	(load-code "fp-functions.lisp")
	(load-code "test-framework.lisp")
	;; (load-code "tests.lisp")
	(load-code "fp-interpreter.lisp" nil t)
)