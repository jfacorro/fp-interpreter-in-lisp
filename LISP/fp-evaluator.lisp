(in-package :com.facorro.fp.evaluator)
;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun evaluate (node)
	(cond 
		((null node) nil)
		(t 
			(let* ((data (datum node))
				  (childs (children node))
				  (fn (get-function data)))
				(debug-msg "(evaluate)~%")
				(debug-msg "  data: ~a~%" data)
				(debug-msg "  children: ~a~%" cilds)
				(debug-msg "  fn: ~a~%~%" fn)
				(if (null fn)
					(map-to-value data)
					(if (listp childs)
						(apply (getf fn :function) (mapcar #'evaluate childs))
						(getf fn :function)))))))
;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun map-to-value (data)
	(cond 
		((string= data "<>") nil)
		((numberp (intern data)) (number (intern data)))
		(t (intern data))))