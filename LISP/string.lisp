(in-package :com.facorro.string)
;;----------------------------------------------
;; string-split
;;----------------------------------------------
(defun string-split (str delim)
  (let ((index (search delim str)))
    (cond (index (append (list 	(string-upcase (subseq str 0 index)))
                                (string-split (subseq str (+ index 1)) delim)))
          (t 
           (if (string= "" str) '() (list (string-upcase str)))))))
;;----------------------------------------------
;; string-explode
;;----------------------------------------------
(defun string-explode (str &rest delims)
	"Explodes a given string using the specified delimiters"
	(let (
			(exploded-str 
				(flatten 
					(mapcar 
						(lambda (s) (string-explode-helper s (first delims)))
						(if (atom str) (list str) str)))))
		(cond 
			((null delims) str)
			((= 1 (length delims)) exploded-str)
			(t
				(apply #'string-explode (append (list exploded-str) (rest delims)))))))
;;----------------------------------------------
;; string-explode-helper
;;----------------------------------------------
(defun string-explode-helper (str delim)
  (let ((index (search delim str))
		(delim-length (length delim)))
    (cond ((null index) (if (string= "" str) nil (list (string-upcase str))))
		  ((> index 0) (append 
							(list (string-upcase (subseq str 0 index))) 
							(list delim)
                            (string-explode-helper (subseq str (+ index delim-length)) delim)))
		  ((= index 0) (append 
							(list delim)
                            (string-explode-helper (subseq str (+ index delim-length)) delim))))))
;;----------------------------------------------
;; Flatten
;;----------------------------------------------
(defun flatten (l)
	(cond
		((null l) nil)
		((atom l) (list l))
		(t (append 	(flatten (car l))
					(flatten (cdr l))))))