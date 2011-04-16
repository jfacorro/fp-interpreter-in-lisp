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

;-----------------------------------------
(in-package :com.facorro.debug)
;-----------------------------------------
(debug-on :com.facorro.parser)
;-----------------------------------------
(in-package :com.facorro.fp.parser)
;-----------------------------------------
;(defparameter rule (defrule "Replace strings for parser-friendly expressions"
;		(string-replace arg
;			; For 
;			"[" "( construct ("
;			"]" "))"
;			"," ")(")))
;(getf rule :function)
;(funcall (getf rule :function) "[")
;(parse "/appendr o(alpha(atom=>id;~<>))")
;(parse "((/appendr) o (alpha(atom=>id;~<>)))")
;(parse "id : <1 <2>>")
;-----------------------------------------
(in-package :com.facorro.fp.parser)
;-----------------------------------------
;(listify '("(" "/" "appendr" ")" "o" "(" "alpha" "(" "atom" "=>" "id" "~" "<>" ")" ")"))
;(listify '("(" "1" "(" "2" "(" "4" ")" ")" "(" "5" ")" "3" ")"))
;-----------------------------------------
(in-package :com.facorro.string)
;-----------------------------------------
;(string-explode "11.11.11.1.1111,111" "." ",")
;(string-split "11.11.11.1.1111,111" "." ",")
;(string-replace "aaaabbbbcccc" "aaaa" "123456")
;(string-replace "aaaabbbbcccc" "aa" "123456" "c" " ")
;-----------------------------------------
(in-package :com.facorro.fp.interpreter)
;-----------------------------------------
;(funcall (interpret "id") '1)
;-----------------------------------------
(in-package :com.facorro.fp.interpreter)
;-----------------------------------------
;(funcall (interpret "/ appendl") '(-1 1 1 1 (3 2)))
;(funcall (interpret "[2°, 1°, id]") '(1 2))
;(funcall (interpret "~fdgh") '((1 2) (3 4) (5 6)))
;(funcall (interpret "((eq o [1°, 2°]) => ~1 ; ~2)") '(1 1))
;(funcall (interpret "eq => ~T ; ~<>") '(1 2))
;(funcall (interpret "2° o 2°") '(1 (2 3)))
;(interpret "2° o 2° : <1, <2, 3>, 4>")
;(interpret "2°: <1 <2 3> 4>")
;(get-function "ID")
(interpret "id : <1 2 3>")
;(interpret "def new : eq o [1°, 2°]")
;(interpret "(new) : <1 1>")
;-----------------------------------------
(in-package :com.facorro.fp.repl)

;-----------------------------------------
;(fp-repl)