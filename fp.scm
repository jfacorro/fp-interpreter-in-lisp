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
(define (tlr l)
  (cond ((<= (length l) 1) '())
        (else (append (list (car l)) (tlr (cdr l))))))
(test (tlr '(a b c)))
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
