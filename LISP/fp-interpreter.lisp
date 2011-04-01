;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun fp-interpret (code)
   code)
;;----------------------------------------------
;; tokenize
;;----------------------------------------------
(defun tokenize (str)
        (string-split str #\ ))
;----------------------------------------------
; string-split
;----------------------------------------------
(defun string-split (str delim)
  (let ((index (string-first-index str delim)))
    (cond ((>= index 0) (append (list (intern (string-upcase (subseq str 0 index))))
                                (string-split (subseq str (+ index 1)) delim)))
          (t 
           (if (string= "" str) '() (list (intern (string-upcase str))))))))
;----------------------------------------------
; string-first-index
;----------------------------------------------
(defun string-first-index (str c)
  (string-first-index-helper str c 0))

(defun string-first-index-helper (str c index)
  (cond ((string= str "") -1)
        (t
         (if (char= c (char str 0))
             index 
             (string-first-index-helper (subseq str 1) c (+ 1 index))))))
;----------------------------------------------
(tokenize "Juan Martin")
(string-split "id o 1" #\ )
;(map-symbol-to-function '/)
(fp-interpret '(id o 1))
