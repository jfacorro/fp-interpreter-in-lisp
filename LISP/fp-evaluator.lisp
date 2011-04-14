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
;; Evaluates the FP env
;;---------------------------------------
(defun evaluate-env (env)
	(symbolize env))
;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun map-to-value (data)
	"Maps a string to a value"
	(cond 
		((string= data "<>") nil)
		((numericp data) (parse-integer data :junk-allowed t))
		(t (intern (string-upcase data)))))
;;----------------------------------------------
;; numericp
;;----------------------------------------------
(defun numericp (str)
	"Returns true if the string is a integer"
	(not (null (parse-integer str :junk-allowed t))))
;;----------------------------------------------
;; Symbolize
;;----------------------------------------------
(defun symbolize (data)
	"Converts all string elements in symbols or numbers"
	(cond ((null data) data)
		((atom data) (map-to-value data))
		(t
			(mapcar #'symbolize data))))






