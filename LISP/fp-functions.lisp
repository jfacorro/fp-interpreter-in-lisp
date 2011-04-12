(in-package :com.facorro.fp.functions)
;;----------------------------------------------
;; General hash table function
;;----------------------------------------------
(defun add-hash-item (hash-table key value)
	(setf (gethash key hash-table) value))
;;----------------------------------------------
;; Define functions hash
;;----------------------------------------------
(defparameter *functions* (make-hash-table :test #'equal))
;;----------------------------------------------
;; Add functions to hash-tables
;;----------------------------------------------
(defun add-function (fp-function)
	(add-hash-item *functions* (getf fp-function :name) fp-function))
;;----------------------------------------------
;; Get function
;;----------------------------------------------
(defun get-function (name)
	(let ((fn (gethash name *functions*)))
		(if (null fn) name fn)))
;;----------------------------------------------
;; most-precedence
;;----------------------------------------------
(defun most-precedence (n1 n2)
	(let ((p1 (getf (get-function n1) :nparams))
		  (p2 (getf (get-function n2) :nparams)))
		(> n1 n2)))
;; ----------------------------------------
;; def-fp-function
;; ----------------------------------------
(defun def-fp-function (name numparam func)
	(list :name (string-upcase name) :nparam numparam :function func))
;;------------------------------------
;; noparams-p
;;------------------------------------
(defun noparams-p (fp-fun)
	(= (getf fp-fun :nparam) 0))
;;----------------------------------------------
;; FP functions hash
;;----------------------------------------------
(add-function (def-fp-function "id" 0 #'id))
(add-function (def-fp-function "n" 1 #'selector))
(add-function (def-fp-function "nr" 1 #'selector-right))
(add-function (def-fp-function "tl" 0 #'tl))
(add-function (def-fp-function "tlr" 0 #'tlr))
(add-function (def-fp-function "atom" 0 #'fp-atom))
(add-function (def-fp-function "eq" 0 #'fp-eq))
(add-function (def-fp-function "null" 0 #'fp-null))
(add-function (def-fp-function "reverse" 0 #'fp-reverse))
(add-function (def-fp-function "iota" 0 #'iota))
(add-function (def-fp-function "distl" 0 #'distl))
(add-function (def-fp-function "distr" 0 #'distr))
(add-function (def-fp-function "length" 0 #'fp-length))
(add-function (def-fp-function "-" 0 #'fp--))
(add-function (def-fp-function "+" 0 #'fp-+))
(add-function (def-fp-function "*" 0 #'fp-*))
(add-function (def-fp-function "%" 0 #'fp-%))
(add-function (def-fp-function "<" 0 #'fp-<))
(add-function (def-fp-function ">" 0 #'fp->))
(add-function (def-fp-function "trans" 0 #'trans))
(add-function (def-fp-function "and" 0 #'fp-and))
(add-function (def-fp-function "or" 0 #'fp-or))
(add-function (def-fp-function "not" 0 #'fp-not))
(add-function (def-fp-function "appendl" 0 #'appendl))
(add-function (def-fp-function "appendr" 0 #'appendr))
(add-function (def-fp-function "rot" 0 #'rot))
(add-function (def-fp-function "rotr" 0 #'rotr))
;;------------------------------------
;; Functional forms
;;------------------------------------
(add-function (def-fp-function "o" 2 #'compose))
(add-function (def-fp-function "[" -1 #'construct))
(add-function (def-fp-function "~" 1 #'const))
(add-function (def-fp-function "->" 3 #'fp-cond))
(add-function (def-fp-function "/" 1 #'insert))
(add-function (def-fp-function "alpha" 1 #'alpha))