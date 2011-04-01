; test
(defun is-list (arg) (not (atom arg)))

(defmacro test (fun &rest args)
  `(progn 
     (display-expr ',fun ',args)
     (let ((result (apply ,fun ',args)))
       (display-result result))))

(defun display-expr (expr args)
  (format t "Expression: (~a ~a)~%" expr args))

(defun display-result (result)
  (format t "Result: ~a~%" result))