; test
(defun is-list (arg) (not (atom arg)))

(defmacro test (fun args)
  ''fun 
  `,args)
