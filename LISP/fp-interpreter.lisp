(in-package :com.facorro.lisp.fp.interpreter)
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
   (mapcar #'get-function (tokenize code)))
;;----------------------------------------------
;; tokenize
;;----------------------------------------------
(defun tokenize (str)
        (mapcar #'intern (string-split str #\ )))
;;----------------------------------------------
;; string-split
;;----------------------------------------------
(defun string-split (str delim)
  (let ((index (position delim str)))
    (cond (index (append (list 	(string-upcase (subseq str 0 index)))
                                (string-split (subseq str (+ index 1)) delim)))
          (t 
           (if (string= "" str) '() (list (string-upcase str)))))))
;;----------------------------------------------
(test tokenize "Juan Martin")
(string-split "id o 1" #\ )
(get-function '/)
(test interpret "id o 1")
