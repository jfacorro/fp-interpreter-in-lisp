(defun fp-interpret (code)
   code)

(defun tokenize (str)
        (string-split str #\ ))
;----------------------------------------------
; string-split
;----------------------------------------------
(defun string-split (str delim)
  (let ((index (string-first-index str delim)))
    (cond ((>= index 0) (append (list (subseq str 0 index)) (string-split (subseq str (+ index 1)) delim)))
          (t 
           (if (string= "" str) ''() (list str))))))
;----------------------------------------------
; string-first-index
;----------------------------------------------
(defun string-first-index (str char)
  (string-first-index-helper str char 0))

(defun string-first-index-helper (str char index)
  (cond ((string= str "") -1)
        (t
         (if (char= char (char str 0))
             index 
             (string-first-index-helper (subseq str 1) char (+ 1 index))))))
;----------------------------------------------

(tokenize "Juan Martin")
(string-split "id o 1" #\ )
;(map-symbol-to-function '/)
(fp-interpret '(id o 1))
