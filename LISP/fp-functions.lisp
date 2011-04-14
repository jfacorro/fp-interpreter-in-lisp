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
(defun def-fp-function (name fn &key (precedence 0) (nparam 0))
	(list :name (string-upcase name) :function fn :precedence precedence :nparam nparam))
;;------------------------------------
;; noparams-p
;;------------------------------------
(defun operand? (fn)
	(or (null fn) (= (getf fn :nparam) 0)))
;;----------------------------------------------
;; FP functions
;;----------------------------------------------
(add-function (def-fp-function "id" #'id))
(add-function (def-fp-function "°" #'selector :precedence 3 :nparam 1))
(add-function (def-fp-function "°r" #'selector-right :precedence 3 :nparam 1))
(add-function (def-fp-function "tl" #'tl))
(add-function (def-fp-function "tlr" #'tlr))
(add-function (def-fp-function "atom" #'fp-atom))
(add-function (def-fp-function "eq" #'fp-eq))
(add-function (def-fp-function "null" #'fp-null))
(add-function (def-fp-function "reverse" #'fp-reverse))
(add-function (def-fp-function "iota" #'iota))
(add-function (def-fp-function "distl" #'distl))
(add-function (def-fp-function "distr" #'distr))
(add-function (def-fp-function "length" #'fp-length))
(add-function (def-fp-function "-" #'fp--))
(add-function (def-fp-function "+" #'fp-+))
(add-function (def-fp-function "*" #'fp-*))
(add-function (def-fp-function "%" #'fp-%))
(add-function (def-fp-function "<" #'fp-<))
(add-function (def-fp-function ">" #'fp->))
(add-function (def-fp-function "trans" #'trans))
(add-function (def-fp-function "and" #'fp-and))
(add-function (def-fp-function "or" #'fp-or))
(add-function (def-fp-function "not" #'fp-not))
(add-function (def-fp-function "appendl" #'appendl))
(add-function (def-fp-function "appendr" #'appendr))
(add-function (def-fp-function "rot" #'rot))
(add-function (def-fp-function "rotr" #'rotr))
;;----------------------------------------------
;; Functional forms
;;----------------------------------------------
(add-function (def-fp-function "o" #'compose :precedence 1 :nparam 2))
(add-function (def-fp-function "construct" #'construct :precedence 0 :nparam -1))
(add-function (def-fp-function "~" #'const :precedence 2 :nparam 1))
(add-function (def-fp-function "=>" #'fp-cond :precedence 0 :nparam 3))
(add-function (def-fp-function "/" #'insert :precedence 2 :nparam 1))
(add-function (def-fp-function "alpha" #'alpha :precedence 2 :nparam 1))
;;----------------------------------------------
;; def function, used to add user defined
;;----------------------------------------------
(add-function (def-fp-function "def" #'def :precedence 0 :nparam 2))