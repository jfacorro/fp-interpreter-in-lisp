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
;(debug-on :com.facorro.parser)
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
;(parse "/appendr o(alpha(atom->id;~<>))")
;(parse "((/appendr) o (alpha(atom->id;~<>)))")
;(parse "id : <1 <2>>")
;-----------------------------------------
(in-package :com.facorro.fp.parser)
;-----------------------------------------
;(listify '("(" "/" "appendr" ")" "o" "(" "alpha" "(" "atom" "->" "id" "~" "<>" ")" ")"))
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
;(get-function "ID")
;(interpret "2# o 2# : <1, <2, 3>, 4>")
;(interpret "2#: <A <T 3> 4>")
;(interpret "id: <A <T 3> 4>")
;(interpret "id: <>")
;(interpret "and: <T T>")
;(interpret "and: <T T F>")
;(interpret "or: <F F F>")
;(interpret "or: <F F T>")
;(interpret ">: <1 2>")
;(interpret "id : <T, <2, 5>, 3>")
;(interpret "1# : <1, 2, 3>")
;(interpret "1#r : <1, 2, 3>")
;(interpret "eq o [1#r, 1#] : <1 2 1>")
;(debug-on :com.facorro.parser)
;(debug-on :com.facorro.interpreter)
;(interpret "def newbla : eq o [1#, 3#]")
;(interpret "newbla : <1 2 1>")
;-----------------------------------------
(in-package :com.facorro.fp.repl)
;-----------------------------------------
(fp-repl)