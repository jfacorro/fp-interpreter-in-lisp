(in-package :com.facorro.test)
;;------------------------------
;; test
;;------------------------------
(defmacro test (fun &rest args)
  `(progn 
     (display-expr ',fun ',args)
     (let ((result (apply (function ,fun) ',args)))
       (display-result result))))
;;------------------------------
;; display-expr
;;------------------------------
(defun display-expr (expr args)
  (format t "Expression: (~a ~a)~%" expr (car args)))
;;------------------------------
;; display-result 
;;------------------------------
(defun display-result (result)
  (format t "Result: ~a~%" result))