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
				(debug-msg "  children: ~a~%" childs)
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
		((numericp data) (parse-integer data :junk-allowed t))
		(t (intern data))))

(defun numericp (str)
	(not (null (parse-integer str :junk-allowed t))))