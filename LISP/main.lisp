(defvar *filepath* "C:\\Juan\\02.Personal\\My Dropbox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP")

(defun load-code (file-name)
		(load (concatenate 'string *filepath* file-name)))

(load-code "fp-functions.lisp")
(load-code "test-framework.lisp")
;(load-code "tests.lisp")
(load-code "fp-interpreter.lisp")