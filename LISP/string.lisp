(in-package :com.facorro.string)
;;----------------------------------------------
;; string-split
;;----------------------------------------------
(defun string-split (str &rest delims)
	"Splits a given string using the specified delimiters"
	(let ((splitted-str (list str)))
		; If there are delimiters then explode
		(if (not (null delims))
			; For each delimiter explode the string 
			; and each instance afterwards
			(dolist (delim delims)
				(setf splitted-str (flatten (mapcar (explode-lambda delim nil) splitted-str)))))
		splitted-str))
;;----------------------------------------------
;; string-explode
;;----------------------------------------------
(defun string-explode (str &rest delims)
	"Explodes a given string using the specified delimiters"
	(let ((exploded-str (if (listp str) str (list str))))
		; If there are delimiters then explode
		(if (not (null delims))
			; For each delimiter explode the string 
			; and each instance afterwards
			(dolist (delim delims)
				(setf exploded-str (flatten (mapcar (explode-lambda delim) exploded-str)))))
		exploded-str))
;;----------------------------------------------
;; explode-lambda
;;----------------------------------------------
(defun explode-lambda (delim &optional (explode t))
	"Generates a function with the delimiter"
	(lambda (str) (string-explode-helper str delim explode)))
;;----------------------------------------------
;; string-explode-helper
;;----------------------------------------------
(defun string-explode-helper (str delim &optional (explode t))
	"Explodes or splits (based on 'explode' param value) a string using the delimiters specified"
	(let ((index (search delim str))
		  (delim-length (length delim)))
		(if (null index)
			(if (empty-string? str) nil (list str))
			(let ((begin-str (subseq str 0 index))
				  (delim (if explode (list delim) nil))
				  (end-str (string-explode-helper (subseq str (+ index delim-length)) delim explode)))

				(append
					; if delimiter is in the first position don't add empty string
					(if (zerop index) nil (list begin-str))
					delim
					end-str)))))
;;----------------------------------------------
;; Flatten
;;----------------------------------------------
(defun flatten (l)
	(cond
		((null l) nil)
		((atom l) (list l))
		(t (append 	(flatten (car l))
					(flatten (cdr l))))))
;;----------------------------------------------
;; empty-string
;;----------------------------------------------
(defun empty-string? (str)
	(string= "" str))
;;----------------------------------------------
;; string-replace
;;----------------------------------------------
(defun string-replace (str &rest substrs)
	"Replace a series of substrings for others by 
	exploding the whole string with the first one, 
	replacing and then concatenating"
	(debug-msg :com.facorro.string "string-replace~%")
	(debug-msg :com.facorro.string "  str: ~a~%" str)
	(debug-msg :com.facorro.string "  substrs: ~a~%" substrs)
	(let* ((substr1 (first substrs))
		   (substr2 (second substrs))
		   (expl-str (string-explode str substr1))
		   (result-str nil))
		; Iterate through the exploded list and replace substr1 for substr2
		(dolist (item expl-str)
			(setf result-str (append result-str (if (string= item substr1) `(,substr2) `(,item)))))
		; Concatenate all loose string in the list
		(setf result-str (apply #'concatenate (append '(string) result-str)))
		; If there are still replacements, keep going
		(setf substrs (cddr substrs))
		(if (not (null substrs))
			(apply #'string-replace (append (list result-str) substrs))
			result-str)))
;;----------------------------------------------
;; string-explode-sequentially
;;----------------------------------------------
(defun string-explode-sequentially (str &rest delims)
	nil)
