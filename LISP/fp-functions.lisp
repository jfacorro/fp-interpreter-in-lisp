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
	(gethash name *functions*))
;; ----------------------------------------
;; def-fp-function
;; ----------------------------------------
(defun def-fp-function (name numparam func)
	(list :name (string-upcase name) :nparam numparam :function func))
;;----------------------------------------------
;; FP functions hash
;;----------------------------------------------
(add-function (def-fp-function "id" 0 #'id))
(add-function (def-fp-function "o" 2 #'compose))
#|
(add-function 'n 		#'selector)
(add-function 'nr 		#'selector-right)
(add-function 'tl 		#'tl)
(add-function 'tlr 		#'tlr)
(add-function 'atom 	#'fp-atom)
(add-function 'eq 		#'fp-eq)
(add-function 'null 	#'fp-null)
(add-function 'reverse	#'fp-reverse)
(add-function 'iota		#'iota)
(add-function 'distl	#'distl)
(add-function 'distr	#'distr)
(add-function 'length	#'fp-length)
(add-function '-		#'fp--)
(add-function '+		#'fp-+)
(add-function '*		#'fp-*)
(add-function '%		#'fp-%)
(add-function '<		#'fp-<)
(add-function '>		#'fp->)
(add-function 'trans	#'trans)
(add-function 'and		#'fp-and)
(add-function 'or		#'fp-or)
(add-function 'not		#'fp-not)
(add-function 'appendl	#'appendl)
(add-function 'appendr	#'appendr)
(add-function 'rot		#'rot)
(add-function 'rotr		#'rotr)
(add-function 'o		#'compose)
(add-function '[]		#'construct)
(add-function '~		#'const)
(add-function 'cond		#'fp-cond)
(add-function '/		#'insert)
(add-function 'alpha	#'alpha)
|#
