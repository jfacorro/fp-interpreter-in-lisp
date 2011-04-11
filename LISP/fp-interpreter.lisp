(in-package :com.facorro.fp.interpreter)
;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
   (mapcar #'get-function (parse code)))
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
#|
(test tokenize "Juan Martin")
(string-split "id o 1" #\ )
(get-function '/)
|#
(parse "id o 1")
(interpret "id o 1")
