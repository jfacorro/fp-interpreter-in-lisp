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
			(if (empty-string? str) nil (list (string-upcase str)))
			(let ((begin-str (string-upcase (subseq str 0 index)))
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
	(debug-msg "empty-string?: ~a~%" str)
	(string= "" str))