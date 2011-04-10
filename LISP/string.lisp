(in-package :com.facorro.string)
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
;;
;;----------------------------------------------
(defun string-explode (str delim)
  (let ((index (position delim str)))
    (cond ((null index) (if (string= "" str) '() (list (string-upcase str))))
		  ((> index 0) (append (list 	(string-upcase (subseq str 0 index))) 
								(list (string delim))
                                (string-explode (subseq str (+ index 1)) delim)))
		  ((= index 0) (append (list (string delim))
                               (string-explode (subseq str (+ index 1)) delim))))))