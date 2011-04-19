(in-package :com.facorro.fp.functions)
;;------------------------------
;; Constante FP values
;;------------------------------
(defconstant *false-value* "F")
(defconstant *true-value* "T")
(defconstant *empty-list-value* "<>")
;;------------------------------
;; id
;;------------------------------
(defun id ()	
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(id) arg: ~a~%" arg)
		arg))
;;------------------------------
;; selector
;;------------------------------
(defun selector (n)
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(selector (~a)) arg: ~a~%" n arg)
		(nth (1- n) arg)))
;;------------------------------
; selector-right
;;------------------------------
(defun selector-right (n)
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(selector-right (~a)) arg: ~a~%" n arg)
		(nth (1- n) (reverse arg))))
;;------------------------------
; tl (tail)
;;------------------------------
(defun tl ()	
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(tl) arg: ~a~%" arg)
		(rest arg)))
;;------------------------------
; tlr (tail right)
;;------------------------------
(defun tlr ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(tlr) arg: ~a~%" arg)
		(reverse (rest (reverse arg)))))
;;------------------------------
; atom
;;------------------------------
(defun fp-atom ()	
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(atom) arg: ~a~%" arg)
		(get-truth-value (atom arg))))
;;------------------------------
; eq
;;------------------------------
(defun fp-eq ()
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(eq) arg: ~a~%" arg)
		(get-truth-value (equal (car arg) (cadr arg)))))
;;------------------------------
; fp-null
;;------------------------------
(defun fp-null ()
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(fp-null) arg: ~a~%" arg)
		(get-truth-value (null arg))))
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
          (t (append (funcall (iota) (- n 1)) (list n))))))
;;------------------------------
; fp-distl
;;------------------------------
(defun distl ()
	(lambda (arg)
		(let ((l (cadr arg))
             (a (car arg)))
			(cond 
				((null l) nil)
				(t
					(append (list (list a (car l))) (funcall (distl) (list a (cdr l)))))))))
;;------------------------------
; fp-distr
;;------------------------------
(defun distr ()
  (lambda (arg)
    (let ((l (car arg))
          (a (cadr arg)))
      (cond ((null l) nil)
            (t
             (append (list (list (car l) a)) (funcall (distr) (list (cdr l) a))))))))
;;------------------------------
; fp-length
;;------------------------------
(defun fp-length ()
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(fp-length) arg: ~a~%" arg)
		(length arg)))
;;------------------------------
; Binary operators
; + - * / < >
;;------------------------------
(defun make-fp-operator (op)
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(~a) arg: ~a~%" op arg)
		(funcall op (car arg) (cadr arg))))
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
  (make-fp-operator (lambda (n1 n2) (floor (/ n1 n2)))))
;;------------------------------
; <
;;------------------------------
(defun fp-< ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(<) arg: ~a~%" arg)
		(get-truth-value (< (first arg) (second arg)))))
	;(make-fp-operator #'<))
;;------------------------------
; >
;;------------------------------
(defun fp-> ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(>) arg: ~a~%" arg)
		(get-truth-value (> (first arg) (second arg)))))
;;------------------------------
; trans
;;------------------------------
(defun trans ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(trans) arg: ~a~%" arg)
		(cond 
			((null (car arg)) nil)
			(t
				(append (list (mapcar #'car arg)) (funcall (trans) (mapcar #'cdr arg)))))))
;;------------------------------
; and
;;------------------------------
(defun fp-and ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(and) arg: ~a~%" arg)
		(get-truth-value (eval (append '(and) (map-truth-values arg))))))
;;------------------------------
;; or
;;------------------------------
(defun fp-or ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(or) arg: ~a~%" arg)
		(get-truth-value (eval (append '(or) (map-truth-values arg))))))
;;------------------------------
;; map-truth-values
;;------------------------------
(defun map-truth-values (arg)
	(mapcar #'get-lisp-truth-value arg))
;;------------------------------
;; get-truth-value
;;------------------------------
(defun get-lisp-truth-value (val)
	(equal val *true-value*))
;;------------------------------
;; get-truth-value
;;------------------------------
(defun get-truth-value (data)
	(if data *true-value* *false-value*))
;;------------------------------
;; not
;;------------------------------
(defun fp-not ()	
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(not) arg: ~a~%" arg)
		(if (not (get-lisp-truth-value arg)) *true-value* *false-value*)))
;;------------------------------
; fp-appendl
;;------------------------------
(defun appendl ()
  (lambda (arg)
	(debug-msg :com.facorro.fp.functions "(appendl) arg: ~a~%" arg)
    (let ((a (car arg))
          (l (cadr arg)))
      (append (list a) l))))
;;------------------------------
; fp-appendr
;;------------------------------
(defun appendr ()
  (lambda (arg)
	(debug-msg :com.facorro.fp.functions "(appendr) arg: ~a~%" arg)
    (let ((l (first arg))
          (a (second arg)))
      (append l (list a)))))
;;------------------------------
; rot
;;------------------------------
(defun rot ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(rot) arg: ~a~%" arg)
		(append (cdr arg) (list (car arg)))))
;;------------------------------
; rotr
;;------------------------------
(defun rotr ()
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(rotr) arg: ~a~%" arg)
		(append (last arg) (butlast arg))))
;;------------------------------
; fp-compose
;;------------------------------
(defun compose (f1 f2)
	(lambda (arg)
		(debug-msg :com.facorro.fp.functions "(compose) arg: ~a~%" arg)
		(fp-funcall f1 (fp-funcall f2 arg))))
;;------------------------------
; fp-construct
;;------------------------------
(defun construct (&rest args)
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(construct) arg: ~a~%" arg)
		(mapcar (lambda (f) 
			(debug-msg :com.facorro.fp.functions " (in construct) (~a) arg: ~a~%" f arg)
			(fp-funcall f arg)) args)))
;;------------------------------
; fp-const
;;------------------------------
(defun const (const)
  (lambda (arg)
	(debug-msg :com.facorro.fp.functions "(const) const: ~a~%" const)
	arg ; To avoid compiler warning
	const))
;;------------------------------
; fp-cond
;;------------------------------
(defun fp-cond (a b c)	
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(->) arg: ~a~%" arg)
		(if (get-lisp-truth-value (fp-funcall a arg)) (fp-funcall b arg) (fp-funcall c arg))))
;;------------------------------
; fp-insert
;;------------------------------
(defun insert (f)
	(lambda (arg) 
		(debug-msg :com.facorro.fp.functions "(insert) arg: ~a~%" arg)
		(cond
			((null arg) nil)
			((= (length arg) 1) (car arg))
			((= (length arg) 2) (fp-funcall f (list (car arg) (cadr arg))))
			(t
				(fp-funcall f (list (car arg) (fp-funcall (insert f) (cdr arg))))))))
;;------------------------------
; alpha
;;------------------------------
(defun alpha (f)
	(lambda (arg) 
		(mapcar (lambda (a) (fp-funcall f a)) arg)))
;;------------------------------
; def
;;------------------------------
(defun def (name fn)
	"Creates a user defined function"
	(when (functionp name) 
		(error (concatenate 'string "The function already exists")))
	(lambda ()
		(add-function (make-function name (make-user-function name fn)))
		(concatenate 'string "FUNCTION " name " DEFINED")))
;;------------------------------
; make fp user function
;;------------------------------
(defun make-user-function (name fn)
	(lambda ()
		(lambda (arg) 
			(debug-msg :com.facorro.fp.functions "(~a) arg: ~a~%" name arg)
			(fp-funcall fn arg))))