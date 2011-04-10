(in-package :com.facorro.lisp.fp.functions)
;;----------------------------------------------
;; General hash table function
;;----------------------------------------------
(defun add-hash-item (hash-table key value)
	(setf (gethash key hash-table) value))
;;----------------------------------------------
;; Define functions hash
;;----------------------------------------------
(defparameter *functions* (make-hash-table))
;;----------------------------------------------
;; Add functions to hash-tables
;;----------------------------------------------
(defun add-function (key value)
	(add-hash-item *functions* key value))
;;----------------------------------------------
;; Get function
;;----------------------------------------------
(defun get-function (function-symbol)
	(gethash function-symbol *functions*))
;;----------------------------------------------
;; FP functions hash
;;----------------------------------------------
(add-function 'id 		#'id)
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