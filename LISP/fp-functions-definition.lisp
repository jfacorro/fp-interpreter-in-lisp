(in-package :com.facorro.fp.functions)
;;------------------------------
;; id
;;------------------------------
(defun id ()	
	(lambda (arg) 
		(debug-msg "(id) arg: ~a~%" arg)
		arg))
;;------------------------------
;; selector
;;------------------------------
(defun selector (n)
  (lambda (arg)
    (cond ((> n 1) (funcall (selector (- n 1)) (cdr arg)))
          (t (car arg)))))
;;------------------------------
; selector-right
;;------------------------------
(defun selector-right (n)
  (lambda (arg)
    (cond ((> (length arg) 0)
           (cond ((= n (length arg)) (car arg))
                 (t (funcall (selector-right n) (cdr arg)))))
          (t '()))))
;;------------------------------
; tl (tail)
;;------------------------------
(defun tl ()
  (lambda (arg) (cdr arg)))
;;------------------------------
; tlr (tail right)
;;------------------------------
(defun tlr ()
  (lambda (arg)
    (cond ((<= (length arg) 1) '())
          (t (append (list (car arg)) (funcall (fp-tlr) (cdr arg)))))))
;;------------------------------
; atom
;;------------------------------
(defun fp-atom ()
  (lambda (arg) (atom arg)))
;;------------------------------
; eq
;;------------------------------
(defun fp-eq ()
  (lambda (arg) (eql (car arg) (cadr arg))))
;;------------------------------
; fp-null
;;------------------------------
(defun fp-null ()
  (lambda (arg) (eql arg '())))
;;------------------------------
; fp-reverse
;;------------------------------
(defun fp-reverse ()
  (lambda (arg) (reverse arg)))
;;------------------------------
; fp-iota
;;------------------------------
(defun iota ()
  (lambda (n)
    (cond ((< n 2) (list 1))
          (t (append (funcall (fp-iota) (- n 1)) (list n))))))
;;------------------------------
; fp-distl
;;------------------------------
(defun distl ()
  (lambda (arg)
    (let ((l (cadr arg))
          (a (car arg)))
      (cond ((funcall (fp-null) l) '())
            (t
             (append (list (list a (car l))) (funcall (fp-distl) (list a (cdr l)))))))))
;;------------------------------
; fp-distr
;;------------------------------
(defun distr ()
  (lambda (arg)
    (let ((l (car arg))
          (a (cadr arg)))
      (cond ((funcall (fp-null) l) '())
            (t
             (append (list (list (car l) a)) (funcall (fp-distr) (list (cdr l) a))))))))
;;------------------------------
; fp-length
;;------------------------------
(defun fp-length ()
  (lambda (arg) (length arg)))
;;------------------------------
; Binary operators
; + - * / < >
;;------------------------------
(defun make-fp-operator (op)
  (lambda (arg) (funcall op (car arg) (cadr arg))))
;;------------------------------
; -
;;------------------------------
(defun fp-- ()
  (make-fp-operator #'-))
;;------------------------------
; +
;;------------------------------
(defun fp-+ ()
  (make-fp-operator #'+))
;;------------------------------
; *
;;------------------------------
(defun fp-* ()
  (make-fp-operator #'*))
;;------------------------------
; % Division
;;------------------------------
(defun fp-% ()
  (make-fp-operator #'/))
;;------------------------------
; <
;;------------------------------
(defun fp-< ()
  (make-fp-operator #'<))
;;------------------------------
; >
;;------------------------------
(defun fp-> ()
  (make-fp-operator #'>))
;;------------------------------
; trans
;;------------------------------
(defun trans ()
  (lambda (arg)
    (cond ((funcall (fp-null) (car arg)) '())
          (t
           (append (list (mapcar #'car arg)) (funcall (fp-trans) (mapcar #'cdr arg)))))))
;;------------------------------
; and
;;------------------------------
(defun fp-and ()
  (lambda (arg) (eval (append (list 'and) arg))))
;;------------------------------
; or
;;------------------------------
(defun fp-or ()
  (lambda (arg) (eval (append (list 'or) arg))))
;;------------------------------
; not
;;------------------------------
(defun fp-not ()
  (lambda (arg) (not arg)))
;;------------------------------
; fp-appendl
;;------------------------------
(defun appendl ()
  (lambda (arg)
    (let ((a (car arg))
          (l (cadr arg)))
      (cons a l))))
;;------------------------------
; fp-appendr
;;------------------------------
(defun appendr ()
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (append l (list a)))))
;;------------------------------
; rot
;;------------------------------
(defun rot ()
  (lambda (arg)
    (funcall (fp-appendr) 
             (list (cdr arg) (car arg)))))
;;------------------------------
; rotr
;;------------------------------
(defun rotr ()
  (lambda (arg)
    (funcall (fp-appendl) 
             (list (funcall (fp-selector-right 1) arg) 
                   (funcall (fp-tlr) arg)))))
;;------------------------------
; fp-compose
;;------------------------------
(defun compose (f1 f2)
  (lambda (l) (funcall f1 (funcall f2 l))))
;;------------------------------
; fp-construct
;;------------------------------
(defun construct (&rest args)
	(lambda (arg) (mapcar (lambda (f) (funcall f arg)) args)))
;;------------------------------
; fp-const
;;------------------------------
(defun const (const)
  (lambda (arg) 
	arg ; To avoid compiler warning and mantain homogeinity
	const))
;;------------------------------
; fp-cond
;;------------------------------
(defun fp-cond (a b c)
  (lambda (arg) (if (funcall a arg) (funcall b arg) (funcall c arg))))
;;------------------------------
; fp-insert
;;------------------------------
(defun insert (f)
  (lambda (arg) (cond
                  ((funcall (fp-null) arg) '())
                  ((= (length arg) 1) (car arg))
                  ((= (length arg) 2) (funcall f (list (car arg) (cadr arg))))
                  (t
                   (funcall f (list (car arg) (funcall (insert f) (cdr arg))))))))
;;------------------------------
; alpha
;;------------------------------
(defun alpha (f)
  (lambda (l) (mapcar f l)))
;;------------------------------
; def
;;------------------------------
(defun def (name fn)
	"Creates a user defined function"
	(lambda () 
		(add-function (def-fp-function name fn))))