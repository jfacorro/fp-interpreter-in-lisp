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
				(debug-msg "(evaluate) data: ~a - children: ~a - fn: ~a~%" data childs fn)
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
					