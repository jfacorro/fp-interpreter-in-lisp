; test
(defun is-list (arg) (not (atom arg)))

(defmacro test (fun &rest args)
  `(progn (print (quote ,fun))
     (print (quote ,args))))
     ;(apply #',fun ,args)))