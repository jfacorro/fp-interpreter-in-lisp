(in-package :com.facorro.fp.interpreter)
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
	(let ((fn (evaluate (parse code))))
		(debug-msg "~a~%" fn)
		fn))