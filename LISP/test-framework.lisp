; test
(defun is-list (arg) (not (atom arg)))

(defmacro test (fun &rest args)
  (cond ((is-list args) '(fun args))))