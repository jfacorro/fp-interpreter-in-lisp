(in-package :com.facorro.fp.interpreter)
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
	(let* ((parse-result (parse code))
		   (fn (getf parse-result :fn))
		   (env (getf parse-result :env)))
		; Evaluate parse-tree
		(setf fn (evaluate fn))
		; Evaluate env
		(setf env (evaluate-env env))
		(debug-msg "fn: ~a~%" fn)
		(debug-msg "env: ~a~%" env)
		; If there an environment argument then 
		; wrap in a list so that 'apply' works
		(if (not (null env)) 
			(setf env (list env)))
		(apply fn env)))