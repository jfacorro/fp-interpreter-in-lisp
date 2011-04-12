(in-package :com.facorro.fp.repl)
;----------------------------------------------------
;(defmacro while (test &body) `(do ,test ))
;----------------------------------------------------
(defun fp-repl ()
	(format t "~%")
	(let ((expr ""))
		(loop (if (string= expr "quit") (return))
			(format t "FP-LISP> ")
			(setf expr (read-line))
			(format t "Input: ~a~%" expr)
			(interpret expr))))
;----------------------------------------------------