(in-package :com.facorro.fp.repl)
;;----------------------------------------------------
;;;define commands
;;----------------------------------------------------
(defconstant *quit* "quit")
(defconstant *load* "load")
(defconstant *help* "help")
;;----------------------------------------------------
;;;fp-repl
;;;---------------------------------------------------
(defun fp-repl ()
	(format t "~%")
	(loop
		(show-prompt)
		(let ((expr (read-line)))
			(cond 
				((quit? expr) (return "Bye"))
				(t (handle-input expr))))))
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
			((equal cmd *help*) (help))
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
	(format t "FP Interpreter (in LISP)~%")
	(format t "Type:~%")
	(format t "	help				for this information.~%")
	(format t "	load [filepath]	to load a file containing FP code in the environment.~%")
	(format t "	FP expression		to be evaluated. It can be:~%")
	(format t "						 - User function defintion.~%")
	(format t "						 - FP expresion with it's argument.~%"))
;;---------------------------------------------------
;; interpret-and-print
;;---------------------------------------------------
(defun interpret-and-print (expr)
	(format t "~a~%" (interpret expr)))
;;---------------------------------------------------
;; help
;;---------------------------------------------------
(defun load-script (expr)
	(let ((file-path (second expr)))
		(format t "Loading fp script '~a' into envinronment...~%" file-path)
		(with-open-file (file file-path :direction :INPUT :if-does-not-exist nil)						
			(cond 
				((null file) (format t "The file '~a' does not exist.~%" file-path))
				(t 
					(do ((line (read-line file nil) (read-line file nil)))
						((null line))
						(interpret-and-print line))
					(format t "Script loaded...~%"))))))