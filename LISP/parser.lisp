(in-package :com.facorro.parser)
;:--------------------------------------
;; List that contains all parsing rules
;;--------------------------------------
(defparameter *rules* nil)
;;--------------------------------------
;; add-rule
;;--------------------------------------
(defun add-rule (rule)
	"Adds a rule to the parsing rules list"
	(setf *rules* (append  *rules* (list rule))))
;;--------------------------------------
;; apply-all-rules
;;--------------------------------------
(defun parse (raw-code)
	"Applies rules in the order they were added and returns the last result"
	(if (null *rules*) raw-code (apply-rules raw-code *rules*)))

(defun apply-rules (code rules)
	(let* ((rule (car rules))
		(remain (rest rules))
		(changed-code (apply-rule rule code)))
		
		(if (null remain) changed-code (apply-rules changed-code remain))))
;;--------------------------------------
;; apply-rule
;;--------------------------------------
(defun apply-rule (rule raw-code)
	(funcall (getf rule :function) raw-code))
;;--------------------------------------
;; defrule
;;--------------------------------------
(defun defrule (name func)
	(list :name name :function func))
;;--------------------------------------