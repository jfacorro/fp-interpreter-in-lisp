(in-package :com.facorro.fp.interpreter)
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
	(handler-case 
		(let* ((parse-result (parse code))
			   (fn (getf parse-result :fn))
			   (env (getf parse-result :env)))
			; Evaluate parse-tree
			(setf fn (resolve-operand (evaluate fn)))
			; Evaluate env
			(setf env (evaluate-env env))

			(debug-msg :com.facorro.fp.interpreter "fn: ~a~%" fn)
			(debug-msg :com.facorro.fp.interpreter "env: ~a~%" env)

			(unless (null fn) (desymbolize (apply fn env))))
		(condition (c) (format t "~a~%" c))))