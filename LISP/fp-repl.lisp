(in-package :com.facorro.fp.repl)
;;----------------------------------------------------
;;;define commands
;;----------------------------------------------------
(defconstant *quit* "quit")
(defconstant *load* "load")
(defconstant *debug* "debug")
(defconstant *reset* "reset")
(defconstant *help* "help")
;;----------------------------------------------------
;;;fp-repl
;;;---------------------------------------------------
(defun fp-repl ()
	(welcome-msg)
	(loop
		(show-prompt)
		(let ((expr (read-line)))
			(cond 
				((quit? expr) (return "Bye"))
				(t (handle-input expr))))))
;;---------------------------------------------------
;;show-prompt
;;---------------------------------------------------
(defun welcome-msg ()
	(format t "-----------------------------------~%")
	(format t "Welcome to the lisp FP interpreter!~%")
	(format t "-----------------------------------~%")
	(format t "You can type 'help' tp get some directions.~%"))
;;---------------------------------------------------
;;show-prompt
;;---------------------------------------------------
(defun show-prompt ()
	(format t "FP> "))
;;---------------------------------------------------
;;handle-command
;;---------------------------------------------------
(defun handle-input (expr)
	(let* ((expl-expr (string-split expr " "))
		    (cmd (first expl-expr)))
		(cond 
			((empty-string? expr) nil)
			((equal cmd *help*) (help))
			((equal cmd *reset*) (reset))
			((equal cmd *debug*) (debug-on-off))
			((equal cmd *load*) (load-script expl-expr))
			(t (interpret-and-print expr)))))
;;---------------------------------------------------
;;quit?
;;---------------------------------------------------
(defun quit? (expr)
	(equal expr *quit*))
;;---------------------------------------------------
;; help
;;---------------------------------------------------
(defun help ()
	(format t "-----------------------------------~%")
	(format t "FP Interpreter (in LISP)~%")
	(format t "-----------------------------------~%")
	(format t "Type:~%")
	(format t "~%")
	(format t "  help             for this information.~%")
	(format t "~%")
	(format t "  load [filepath]  to load a file containing FP code into the environment.~%")
	(format t "~%")
	(format t "  debug            switches debugging messages on and off.~%")
	(format t "~%")
	(format t "  reset            deletes all user defined functions.~%")
	(format t "~%")
	(format t "  quit             to exit the interpreter.~%")
	(format t "~%")
	(format t "  [expression]     to be evaluated. It can be:~%")
	(format t "                     - User function defintion.~%")
	(format t "                     - FP expresion with it's argument.~%")
	(format t "~%"))
;;---------------------------------------------------
;; interpret-and-print
;;---------------------------------------------------
(defun interpret-and-print (expr)
	(let ((result (interpret expr)))
		(unless (null result) (format t "~a~%" result))))
;;---------------------------------------------------
;; help
;;---------------------------------------------------
(defun load-script (expr)
	(let ((file-path (second expr)))
		(format t "Loading fp script '~a' into environment...~%" file-path)
		(with-open-file (file file-path :direction :INPUT :if-does-not-exist nil)						
			(cond 
				((null file) (format t "The file '~a' does not exist.~%" file-path))
				(t 
					(do ((line (read-line file nil) (read-line file nil)))
						((null line))
						(format t "~a~%" line)
						(handle-input line))
					(format t "Script loaded...~%"))))))
;;---------------------------------------------------
;; debug-on-off
;;---------------------------------------------------
(defun debug-on-off ()
	(cond 
		((debugging? :com.facorro.fp.functions)
			(format t "Debugging off~%")
			(debug-off :com.facorro.fp.functions))
		(t	
			(format t "Debugging on~%")	
			(debug-on :com.facorro.fp.functions))))
;;---------------------------------------------------
;; reset
;;---------------------------------------------------
(defun reset ()
	(format t "Environment reset...~%")
	(reset-functions))