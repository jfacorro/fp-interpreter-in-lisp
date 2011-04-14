(in-package :com.facorro.fp.repl)
;----------------------------------------------------
;(defmacro while (test &body) `(do ,test ))
;----------------------------------------------------
(defun fp-repl ()
	(format t "~%")
	(let ((expr "")
		  (quit-expr "quit"))
		(loop (if (string= expr quit-expr) (return "Bye"))
			(format t "FP> ")
			(setf expr (read-line))
			(format t "Input: ~a~%" expr)
			(if (not (string= expr quit-expr))
				(format t "Result: ~a~%" (interpret expr))))))
;----------------------------------------------------