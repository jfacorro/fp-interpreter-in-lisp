#lang scheme
; test
(define-syntax test
  (syntax-rules ()
    ((test expr) 
     (begin (display 'expr)
            (newline)
            expr))))
;(define (read-fp filename)
;  (let ((port (open-input-file filename)))
;    (read-line port)))
;(read-fp "fp.txt")
; + - / *
; id
(define (fp-id p) p)
(test (fp-id 'abc))
; selector
(define (fp-selector n l)
  (cond ((> n 1) (fp-selector (- n 1) (cdr l)))
        (else (car l))))
(test (fp-selector 2 '(a b c)))
; selector-right
(define (fp-selector-right n l)
  (cond ((> (length l) 0)
         (cond ((= n (length l)) (car l))
                (else (fp-selector-right n (cdr l)))))
        (else '())))
(test (fp-selector-right 3 '(a b c)))
; tl (tail)
(define (fp-tl l)
  (cdr l))
(test (fp-tl '(a b c)))
; tlr (tail right)
(define (fp-tlr l)
  (cond ((<= (length l) 1) '())
        (else (append (list (car l)) (fp-tlr (cdr l))))))
(test (fp-tlr '(a b c)))
; atom
(define (fp-atom a)
  (not (list? a)))
(test (fp-atom 0))
(test (fp-atom '(1 2 3)))
; eq
(define (fp-eq a b)
  (equal? a b))
(test (fp-eq 'b 'a))
(test (fp-eq 'a 'a))
(test (fp-eq 2 1))
(test (fp-eq 1 1))
; <
; >
; fp-null
(define (fp-null a)
  (fp-eq a null))
(test (fp-null '()))
(test (fp-null '(1 3)))
(test (fp-null 1))
; fp-reverse
(define (fp-reverse l) (reverse l))
(test (fp-reverse '(a b c)))
; fp-iota
(define (fp-iota n)
  (cond ((< n 2) (list 1))
        (else (append (fp-iota (- n 1)) (list n)))))
(test (fp-iota 3))
(test (fp-iota 5))
; fp-distl
(define (fp-distl a l)
  (cond ((fp-null l) null)
        (else
         (append (list (list a (car l))) (fp-distl a (cdr l))))))
(test (fp-distl 'a '(b c d)))
(test (fp-distl 'a '()))
; fp-distr
(define (fp-distr l a)
  (cond ((fp-null l) null)
        (else
         (append (list (list (car l) a)) (fp-distr (cdr l) a)))))
(test (fp-distr '(b c d) 'a ))
(test (fp-distr '() 'a ))
; length
; + - * /
; trans
(define (fp-trans l)
  (cond ((fp-null (car l)) null)
        (else
         (append (list (map car l)) (fp-trans (map cdr l))))))
(test (fp-trans '((1 2) (3 4) (5 6))))
; and or not
; fp-appendl
(define (fp-appendl a l) 
  (cons a l))
(test (fp-appendl 'a '(b c)))
; fp-appendr
(define (fp-appendr l a) 
  (append l (list a)))
(test (fp-appendr '(b c) '(a)))
(test (fp-appendr '(b c) 'a))
; rot
(define (fp-rot l)
  (fp-appendr (cdr l) (car l)))
(test (fp-rot '(1 2 3 4)))
; rotr
(define (fp-rotr l)
  (fp-appendl  (fp-selector-right 1 l) (fp-tlr l)))
(test (fp-rotr '(1 2 3 4)))
; fp-compose
(define (fp-compose f1 f2)
  (lambda (l)(apply  f1 (list (apply f2 l)))))
(test ((fp-compose * -) '(2 1)))
; fp-construct
(define (fp-construct . args)
  args)
(test (fp-construct 1 2))
; fp-const
(define (fp-const arg)
  arg)
(test (fp-const 'arg))
; fp-cond
(define (fp-cond c t f)
  (if c t f))
(test (fp-cond (= 1 2) 'uno 'no-uno))
(test (fp-cond (= 1 1) 'uno 'no-uno))
; fp-insert
(define (fp-insert f)
  (lambda (arg) (cond 
                  ((fp-null arg) null)
                  ((= (length arg) 1) (car arg))
                  ((= (length arg) 2) (f (car arg) (cadr arg)))
                  (else
                   (f (car arg) ((fp-insert f) (cdr arg)))))))
(test ((fp-insert fp-appendl) '(1 2 3 ())))
; 