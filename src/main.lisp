;(defparameter *filepath* "C:\\Juan\\02.Personal\\Dropbox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\LISP\\")
(defparameter *filepath* "C:\\JF\\My DropBox\\Facultad\\2011.1er.75.14.Lenguajes.Formales\\fp-interpreter-in-lisp\\")
;----------------------------------------------------
(defun compile-load (file-name &optional (v nil) (p nil))
		(load (compile-file (format nil "~asrc\\~a" *filepath* file-name) :verbose v :print p) :verbose v :print p))
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
	(compile-load "fp-interpreter.lisp" nil nil)
	(compile-load "fp-evaluator.lisp")
	(compile-load "fp-repl.lisp" nil nil))

;--------------------------------------
; Main
;--------------------------------------
(defun main ()
	(com.facorro.fp.repl:fp-repl)
	(EXT:EXIT))
;--------------------------------------
; Generate executable
;--------------------------------------
(EXT:SAVEINITMEM (format nil "~abin\\fp-interpreter.exe" *filepath*)
                 :QUIET t
                 :INIT-FUNCTION 'main
                 :EXECUTABLE t
                 :NORC t)