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
				(debug-msg :com.facorro.fp.evaluator "(evaluate)~%")
				(debug-msg :com.facorro.fp.evaluator "  data: ~a~%" data)
				(debug-msg :com.facorro.fp.evaluator "  children: ~a~%" childs)
				(debug-msg :com.facorro.fp.evaluator "  fn: ~a~%" fn)
				(cond
					((null fn) 
						(debug-msg :com.facorro.fp.evaluator "  mapping to value ~a~%" data)
						(map-to-value data))
					((operand? fn)
						(debug-msg :com.facorro.fp.evaluator "  returning ~a~%" (apply (getf fn :function) nil))
						data)
					(t 
						(debug-msg :com.facorro.fp.evaluator "  calling ~a with ~a~%" data childs)
						(apply (getf fn :function) (mapcar #'evaluate childs))))))))
;;----------------------------------------
;; Evaluates the FP env
;;---------------------------------------
(defun evaluate-env (env)
	(symbolize env))
;;----------------------------------------
;; Maps an FP string to a value
;;----------------------------------------
(defun map-to-value (data)
	"Maps a string to a value"
	(cond 
		((string= data "<>") nil)
		((numericp data) (parse-integer data :junk-allowed t))
		(t 
			(cond 
				((equal *true-value* data) *true-value*)
				((equal *false-value* data) *false-value*)
				((equal *empty-list-value* data) *empty-list-value*)
				(t data)))))					
;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun map-to-fp-value (data)
	(cond 
		((null data) *empty-list-value*)
		((numberp data) (write-to-string data))
		(t (string data))))
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
	"Converts all string elements into symbols or numbers"
	(cond 
		((null data) data)
		((atom data) (map-to-value data))
		(t (mapcar #'symbolize data))))
;;----------------------------------------------
;; desymbolize
;;----------------------------------------------
(defun desymbolize (data)
	(cond 
		((null data) (map-to-fp-value data))
		((atom data) (map-to-fp-value data))
		(t 
			(let ((result nil))
				(setf result (mapcar #'desymbolize data))
				(setf result (insert-between result ", "))
				(setf result (append '("<") result '(">")))
				(setf result (flatten result))
				(setf result (apply #'concatenate (append '(string) result)))))))