#lang scheme
(provide fp-id)
(provide fp-selector)
(provide fp-selector-right)
(provide fp-tl)
(provide fp-tlr)
(provide fp-atom)
(provide fp-eq)
(provide fp-null)
(provide fp-reverse)
(provide fp-iota)
(provide fp-distl)
(provide fp-distr)
(provide fp-length)
(provide fp--)
(provide fp-+)
(provide fp-*)
(provide fp-%)
(provide fp-<)
(provide fp->)
(provide fp-trans)
(provide fp-and)
(provide fp-or)
(provide fp-not)
(provide fp-appendl)
(provide fp-appendr)
(provide fp-rot)
(provide fp-rotr)
(provide fp-compose)
(provide fp-construct)
(provide fp-const)
(provide fp-cond)
(provide fp-insert)
(provide fp-alpha)
(provide fp-alpha)

; id
(define (fp-id)
  (lambda (arg) arg))
; selector
(define (fp-selector n)
  (lambda (arg)
    (cond ((> n 1) ((fp-selector (- n 1)) (cdr arg)))
          (else (car arg)))))
; selector-right
(define (fp-selector-right n)
  (lambda (arg)
    (cond ((> (length arg) 0)
           (cond ((= n (length arg)) (car arg))
                 (else ((fp-selector-right n) (cdr arg)))))
          (else '()))))
; tl (tail)
(define (fp-tl)
  (lambda (arg) (cdr arg)))
; tlr (tail right)
(define (fp-tlr)
  (lambda (arg)
    (cond ((<= (length arg) 1) '())
          (else (append (list (car arg)) ((fp-tlr) (cdr arg)))))))
; atom
(define (fp-atom)
  (lambda (arg) (not (list? arg))))
; eq
(define (fp-eq)
  (lambda (arg) (equal? (car arg) (cadr arg))))
; fp-null
(define (fp-null)
  (lambda (arg) (equal? arg null)))
; fp-reverse
(define (fp-reverse)
  (lambda (arg) (reverse arg)))
; fp-iota
(define (fp-iota)
  (lambda (n)
    (cond ((< n 2) (list 1))
          (else (append ((fp-iota) (- n 1)) (list n))))))
; fp-distl
(define (fp-distl)
  (lambda (arg)
    (let ((l (second arg))
          (a (first arg)))
      (cond (((fp-null) l) null)
            (else
             (append (list (list a (car l))) ((fp-distl) (list a (cdr l)))))))))
; fp-distr
(define (fp-distr)
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (cond (((fp-null) l) null)
            (else
             (append (list (list (car l) a)) ((fp-distr) (list (cdr l) a))))))))
; fp-length
(define (fp-length)
  (lambda (arg) (length arg)))
; Binary operators
; + - * / < >
(define (make-fp-operator op)
  (lambda (arg) (op (first arg) (second arg))))
; -
(define (fp--)
  (make-fp-operator -))
; +
(define (fp-+)
  (make-fp-operator +))
; *
(define (fp-*)
  (make-fp-operator *))
; % Division
(define (fp-%)
  (make-fp-operator /))
; <
(define (fp-<)
  (make-fp-operator <))
; >
(define (fp->)
  (make-fp-operator >))
; trans
(define (fp-trans)
  (lambda (arg)
    (cond (((fp-null) (car arg)) null)
          (else
           (append (list (map car arg)) ((fp-trans) (map cdr arg)))))))
; and
(define (fp-and)
  (lambda (arg) (and arg)))
; or
(define (fp-or)
  (lambda (arg) (or arg)))
; not
(define (fp-not)
  (lambda (arg) (not arg)))
; fp-appendl
(define (fp-appendl)
  (lambda (arg)
    (let ((a (first arg))
          (l (second arg)))
      (cons a l))))    
; fp-appendr
(define (fp-appendr)
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (append l (list a)))))
; rot
(define (fp-rot)
  (lambda (arg)
    ((fp-appendr) (list (cdr arg) (car arg)))))
; rotr
(define (fp-rotr)
  (lambda (arg)
    ((fp-appendl) (list ((fp-selector-right 1) arg) ((fp-tlr) arg)))))
; fp-compose
(define (fp-compose f1 f2)
  (lambda (l)(f1 (f2 l))))
; fp-construct
(define (fp-construct . args)
  (lambda (arg) (map (lambda (t) (apply t (list arg))) args)))
; fp-const
(define (fp-const const)
  (lambda (arg) const))
; fp-cond
(define (fp-cond c t f)
  (lambda (arg) (if (c arg) (t arg) (f arg))))
; fp-insert
(define (fp-insert f)
  (lambda (arg) (cond
                  (((fp-null) arg) null)
                  ((= (length arg) 1) (car arg))
                  ((= (length arg) 2) (f (list (car arg) (cadr arg))))
                  (else
                   (f (list (car arg) ((fp-insert f) (cdr arg))))))))
; fp-alpha
(define (fp-alpha f)
  (lambda (l) (map f l)))