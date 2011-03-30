#lang scheme
; test
(define-syntax test
  (syntax-rules ()
    ((test fun . args)
     (begin (display '(apply fun 'args))
                                (newline)
                                (apply fun 'args)))))
;(define (read-fp filename)
;  (let ((port (open-input-file filename)))
;    (read-line port)))
;(read-fp "fp.txt")

; id
(define (fp-id)
  (lambda (arg) arg))
(test (fp-id) abc)
; selector
(define (fp-selector n)
  (lambda (arg)
    (cond ((> n 1) ((fp-selector (- n 1)) (cdr arg)))
          (else (car arg)))))
(test (fp-selector 2) (a b c))
; selector-right
(define (fp-selector-right n)
  (lambda (arg)
    (cond ((> (length arg) 0)
           (cond ((= n (length arg)) (car arg))
                 (else ((fp-selector-right n) (cdr arg)))))
          (else '()))))
(test (fp-selector-right 3) (a b c))
; tl (tail)
(define (fp-tl)
  (lambda (arg) (cdr arg)))
(test (fp-tl) (a b c))
; tlr (tail right)
(define (fp-tlr)
  (lambda (arg)
    (cond ((<= (length arg) 1) '())
          (else (append (list (car arg)) ((fp-tlr) (cdr arg)))))))
(test (fp-tlr) (a b c))
; atom
(define (fp-atom)
  (lambda (arg) (not (list? arg))))
(test (fp-atom) 0)
(test (fp-atom) (1 2 3))
; eq
(define (fp-eq)
  (lambda (arg) (equal? (car arg) (cadr arg))))
(test (fp-eq) (b a))
(test (fp-eq) (a a))
(test (fp-eq) (2 1))
(test (fp-eq) (1 1))
; fp-null
(define (fp-null)
  (lambda (arg) (equal? arg null)))
(test (fp-null) ())
(test (fp-null) (1 3))
(test (fp-null) 1)
; fp-reverse
(define (fp-reverse)
  (lambda (arg) (reverse arg)))
(test (fp-reverse) (a b c))
; fp-iota
(define (fp-iota)
  (lambda (n)
    (cond ((< n 2) (list 1))
          (else (append ((fp-iota) (- n 1)) (list n))))))
(test (fp-iota) 3)
(test (fp-iota) 5)
; fp-distl
(define (fp-distl)
  (lambda (arg)
    (let ((l (second arg))
          (a (first arg)))
      (cond (((fp-null) l) null)
            (else
             (append (list (list a (car l))) ((fp-distl) (list a (cdr l)))))))))
(test (fp-distl) (a (b c d)))
(test (fp-distl) (a ()))
; fp-distr
(define (fp-distr)
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (cond (((fp-null) l) null)
            (else
             (append (list (list (car l) a)) ((fp-distr) (list (cdr l) a))))))))
(test (fp-distr) ((b c d) a))
(test (fp-distr) (() a ))
; fp-length
(define (fp-length)
  (lambda (arg) (length arg)))
(test (fp-length) (1 2 3 a b c))
(test (fp-length) ())
; Binary operators
; + - * / < >
(define (make-fp-operator op)
  (lambda (arg) (op (first arg) (second arg))))
; -
(define (fp--)
  (make-fp-operator -))
(test (fp--) (4 2))
; +
(define (fp-+)
  (make-fp-operator +))
(test (fp-+) (4 2))
; *
(define (fp-*)
  (make-fp-operator *))
(test (fp-*) (4 2))
; % Division
(define (fp-%)
  (make-fp-operator /))
(test (fp-%) (4 2))
; <
(define (fp-<)
  (make-fp-operator <))
(test (fp-<) (4 2))
; >
(define (fp->)
  (make-fp-operator >))
(test (fp->) (4 2))
; trans
(define (fp-trans)
  (lambda (arg)
    (cond (((fp-null) (car arg)) null)
          (else
           (append (list (map car arg)) ((fp-trans) (map cdr arg)))))))
(test (fp-trans) ((1 2) (3 4) (5 6)))
; and
(define (fp-and)
  (lambda (arg) (and arg)))
(test (fp-and) (#t #f))
; or
(define (fp-or)
  (lambda (arg) (or arg)))
(test (fp-or) (#t #f))
; not
(define (fp-not)
  (lambda (arg) (not arg)))
(test (fp-not) #t)
(test (fp-not) #f)
; fp-appendl
(define (fp-appendl)
  (lambda (arg)
    (let ((a (first arg))
          (l (second arg)))
      (cons a l))))    
(test (fp-appendl) (a (b c)))
; fp-appendr
(define (fp-appendr)
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (append l (list a)))))
(test (fp-appendr) ((b c) (a)))
(test (fp-appendr) ((b c) a))
; rot
(define (fp-rot)
  (lambda (arg)
    ((fp-appendr) (list (cdr arg) (car arg)))))
(test (fp-rot) (1 2 3 4))
; rotr
(define (fp-rotr)
  (lambda (arg)
    ((fp-appendl) (list ((fp-selector-right 1) arg) ((fp-tlr) arg)))))
(test (fp-rotr) (1 2 3 4))
; fp-compose
(define (fp-compose f1 f2)
  (lambda (l)(f1 (f2 l))))
(test (fp-compose (fp--) (fp-rot)) (2 1))
; fp-construct
(define (fp-construct . args)
  (lambda (arg) (map (lambda (t) (apply t (list arg))) args)))
(test (fp-construct (fp-selector 3) (fp-selector 2)) (1 2 3))
; fp-const
(define (fp-const const)
  (lambda (arg) const))
(test (fp-const 'a) ())
; fp-cond
(define (fp-cond c t f)
  (lambda (arg) (if (c arg) (t arg) (f arg))))

(test (fp-cond (fp-compose (fp-not) (fp-null))
               (fp-const 1)
               (fp-const null)) ())
(test (fp-cond (fp-null) (fp-null) (fp-null)) (1 2))
; fp-insert
(define (fp-insert f)
  (lambda (arg) (cond
                  (((fp-null) arg) null)
                  ((= (length arg) 1) (car arg))
                  ((= (length arg) 2) (f (list (car arg) (cadr arg))))
                  (else
                   (f (list (car arg) ((fp-insert f) (cdr arg))))))))
(test (fp-insert (fp-appendl)) (1 2 3 ()))
; fp-alpha
(define (fp-alpha f)
  (lambda (l) (map f l)))
(test (fp-alpha (fp-null)) (() 1 2))
(test (fp-compose (fp-null) (fp-alpha (fp-null))) (() 1 2))
(test (fp-compose (fp-insert (fp-+)) (fp-iota)) 3)