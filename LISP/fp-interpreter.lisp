(in-package :com.facorro.lisp.fp.interpreter)
;;----------------------------------------------
;; General hash table function
;;----------------------------------------------
(defun add-hash-item (hash-table key value)
	(setf (gethash key hash-table) value))
;;----------------------------------------------
;; Define functions hash
;;----------------------------------------------
(defparameter *fp-functions* (make-hash-table))
;;----------------------------------------------
;; Add functions to hash-tables
;;----------------------------------------------
(defun fp-add-function (key value)
	(add-hash-item *fp-functions* key value))
;;----------------------------------------------
;; Get function
;;----------------------------------------------
(defun fp-get-function (function-symbol)
	(gethash function-symbol *fp-functions*))
#|
(fp-add-function 'id 		#'fp-id)
(fp-add-function 'n 		#'fp-selector)
(fp-add-function 'nr 		#'fp-selector-right)
(fp-add-function 'tl 		#'fp-tl)
(fp-add-function 'tlr 		#'fp-tlr)
(fp-add-function 'atom 		#'fp-atom)
(fp-add-function 'eq 		#'fp-eq)
(fp-add-function 'null 		#'fp-null)
(fp-add-function 'reverse	#'fp-reverse)
(fp-add-function 'iota		#'fp-iota)
(fp-add-function 'distl		#'fp-distl)
(fp-add-function 'distr		#'fp-distr)
(fp-add-function 'length	#'fp-length)
(fp-add-function '-			#'fp--)
(fp-add-function '+			#'fp-+)
(fp-add-function '*			#'fp-*)
(fp-add-function '%			#'fp-%)
(fp-add-function '<			#'fp-<)
(fp-add-function '>			#'fp->)
(fp-add-function 'trans		#'fp-trans)
(fp-add-function 'and		#'fp-and)
(fp-add-function 'or		#'fp-or)
(fp-add-function 'not		#'fp-not)
(fp-add-function 'appendl	#'fp-appendl)
(fp-add-function 'appendr	#'fp-appendr)
(fp-add-function 'rot		#'fp-rot)
(fp-add-function 'rotr		#'fp-rotr)
(fp-add-function 'o			#'fp-compose)
(fp-add-function '[]		#'fp-construct)
(fp-add-function '~			#'fp-const)
(fp-add-function 'cond		#'fp-cond)
(fp-add-function '/			#'fp-insert)
(fp-add-function 'alpha		#'fp-alpha)
|#
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun fp-interpret (code)
   (mapcar #'fp-get-function (tokenize code)))
;;----------------------------------------------
;; tokenize
;;----------------------------------------------
(defun tokenize (str)
        (string-split str #\ ))
;;----------------------------------------------
;; string-split
;;----------------------------------------------
(defun string-split (str delim)
  (let ((index (position delim str)))
    (cond (index (append (list (intern (string-upcase (subseq str 0 index))))
                                (string-split (subseq str (+ index 1)) delim)))
          (t 
           (if (string= "" str) '() (list (intern (string-upcase str))))))))
;;----------------------------------------------
;(test #'tokenize "Juan Martin")
(string-split "id o 1" #\ )
(fp-get-function '/)
(fp-interpret "id o 1")
