;; id
(defun fp-id ()
  (lambda (arg) arg))
;; selector
(defun fp-selector (n)
  (lambda (arg)
    (cond ((> n 1) (funcall (fp-selector (- n 1)) (cdr arg)))
          (t (car arg)))))
; selector-right
(defun fp-selector-right (n)
  (lambda (arg)
    (cond ((> (length arg) 0)
           (cond ((= n (length arg)) (car arg))
                 (t (funcall (fp-selector-right n) (cdr arg)))))
          (t '()))))
; tl (tail)
(defun fp-tl ()
  (lambda (arg) (cdr arg)))
; tlr (tail right)
(defun fp-tlr ()
  (lambda (arg)
    (cond ((<= (length arg) 1) '())
          (t (append (list (car arg)) (funcall (fp-tlr) (cdr arg)))))))
; atom
(defun fp-atom ()
  (lambda (arg) (atom arg)))
; eq
(defun fp-eq ()
  (lambda (arg) (eql (car arg) (cadr arg))))
