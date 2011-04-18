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
;; make-function
;; ----------------------------------------
(defun make-function (name fn &key (precedence 0) (nparam 0))
	(list :name (string-upcase name) :function fn :precedence precedence :nparam nparam))
;;------------------------------------
;; noparams-p
;;------------------------------------
(defun operand? (fn)
	(or (null fn) (= (getf fn :nparam) 0)))
;;----------------------------------------------
;; init-functions
;;----------------------------------------------
(defun init-functions ()
	(add-function (make-function "id" #'id))
	(add-function (make-function "#" #'selector :precedence 3 :nparam 1))
	(add-function (make-function "#r" #'selector-right :precedence 3 :nparam 1))
	(add-function (make-function "tl" #'tl))
	(add-function (make-function "tlr" #'tlr))
	(add-function (make-function "atom" #'fp-atom))
	(add-function (make-function "eq" #'fp-eq))
	(add-function (make-function "null" #'fp-null))
	(add-function (make-function "reverse" #'fp-reverse))
	(add-function (make-function "iota" #'iota))
	(add-function (make-function "distl" #'distl))
	(add-function (make-function "distr" #'distr))
	(add-function (make-function "length" #'fp-length))
	(add-function (make-function "-" #'fp--))
	(add-function (make-function "+" #'fp-+))
	(add-function (make-function "*" #'fp-*))
	(add-function (make-function "%" #'fp-%))
	(add-function (make-function "<" #'fp-<))
	(add-function (make-function ">" #'fp->))
	(add-function (make-function "trans" #'trans))
	(add-function (make-function "and" #'fp-and))
	(add-function (make-function "or" #'fp-or))
	(add-function (make-function "not" #'fp-not))
	(add-function (make-function "appendl" #'appendl))
	(add-function (make-function "appendr" #'appendr))
	(add-function (make-function "rot" #'rot))
	(add-function (make-function "rotr" #'rotr))
	;;----------------------------------------------
	;; Functional forms
	;;----------------------------------------------
	(add-function (make-function "o" #'compose :precedence 1 :nparam 2))
	(add-function (make-function "construct" #'construct :precedence 0 :nparam -1))
	(add-function (make-function "~" #'const :precedence 2 :nparam 1))
	(add-function (make-function "->" #'fp-cond :precedence 0 :nparam 3))
	(add-function (make-function "/" #'insert :precedence 2 :nparam 1))
	(add-function (make-function "alpha" #'alpha :precedence 2 :nparam 1))
	;;----------------------------------------------
	;; def function, used to add user defined
	;;----------------------------------------------
	(add-function (make-function "def" #'def :precedence 0 :nparam 2)))
;;----------------------------------------------
;; reset-functions
;;----------------------------------------------
(defun reset-functions ()
	(defparameter *functions* (make-hash-table :test 'equal))
	(init-functions))
;;----------------------------------------------
;; Call init-functions
;;----------------------------------------------
(init-functions)