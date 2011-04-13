(in-package :com.facorro.fp.functions)
;;----------------------------------------------
;; General hash table function
;;----------------------------------------------
(defun add-hash-item (hash-table key value)
	(setf (gethash key hash-table) value))
;;----------------------------------------------
;; Define functions hash
;;----------------------------------------------
(defparameter *functions* (make-hash-table :test 'equal))
;;----------------------------------------------
;; Add functions to hash-tables
;;----------------------------------------------
(defun add-function (fun)
	(add-hash-item *functions* (getf fun :name) fun))
;;----------------------------------------------
;; Get function
;;----------------------------------------------
(defun get-function (name)
	(gethash name *functions*))
;;----------------------------------------------
;; precedence
;;----------------------------------------------
(defun precedence (fn)
	(getf fn :precedence))
;;----------------------------------------------
;; num-params
;;----------------------------------------------
(defun num-params (fn)
	(getf fn :nparam))
;; ----------------------------------------
;; def-fp-function
;; ----------------------------------------
(defun def-fp-function (name precedence fn &optional (nparam 0))
	(list :name (string-upcase name) :precedence precedence :function fn :nparam nparam))
;;------------------------------------
;; noparams-p
;;------------------------------------
(defun operand? (fn)
	(or (null fn) (= (getf fn :nparam) 0)))
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
(add-function (def-fp-function "o" 1 #'compose 2))
(add-function (def-fp-function "construct" 2 #'construct -1))
(add-function (def-fp-function "~" 2 #'const 1))
(add-function (def-fp-function "->" 0 #'fp-cond 3))
(add-function (def-fp-function "/" 2 #'insert 1))
(add-function (def-fp-function "alpha" 2 #'alpha 1))