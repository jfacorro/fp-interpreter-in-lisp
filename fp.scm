#lang scheme
; test
(define-syntax test
  (syntax-rules ()
    ((test expr) 
     (begin (display 'expr)
            (newline)
            expr))))
(define (read-fp filename)
  (let ((port (open-input-file filename)))
    (read-line port)))
(read-fp "fp.txt")
; + - / *
; id
(define (fp-id p)
  p)
(test (id 'abc))
; selector
(define (selector n l)
  (cond ((> n 1) (selector (- n 1) (cdr l)))
        (else (car l))))
(test (selector 2 '(a b c)))
; selector-right
(define (selector-right n l)
  (cond ((> (length l) 0)
         (cond ((= n (length l)) (car l))
                (else (selector-right n (cdr l)))))
        (else '())))
(test (selector-right 3 '(a b c)))
; tl (tail)
(define (tl l)
  (cdr l))
(test (tl '(a b c)))
; tlr (tail right)
(define (tlr l)
  (cond ((<= (length l) 1) '())
        (else (append (list (car l)) (tlr (cdr l))))))
(test (tlr '(a b c)))
; atom
(define (atom a)
  (not (list? a)))
(test (atom 0))
(test (atom '(1 2 3)))
; eq
(define (eq a b)
  (equal? a b))
(test (eq 'b 'a))
(test (eq 'a 'a))
(test (eq 2 1))
(test (eq 1 1))
; <
; >
; is-null
(define (is-null a)
  (eq a null))
(test (is-null '()))
(test (is-null '(1 3)))
(test (is-null 1))
; reverse
; iota
(define (iota n)
  (cond ((< n 2) (list 1))
        (else (append (iota (- n 1)) (list n)))))
(test (iota 3))
(test (iota 5))
; distl
(define (distl a l)
  (cond ((is-null l) null)
        (else
         (append (list (list a (car l))) (distl a (cdr l))))))
(test (distl 'a '(b c d)))
(test (distl 'a '()))
; distr
(define (distr l a)
  (cond ((is-null l) null)
        (else
         (append (list (list (car l) a)) (distr (cdr l) a)))))
(test (distr '(b c d) 'a ))
(test (distr '() 'a ))
; length
; + - * /
; trans
(define (trans l)
  (cond ((is-null (car l)) null)
        (else
         (append (list (map car l)) (trans (map cdr l))))))
(test (trans '((1 2) (3 4) (5 6))))
; and or not
; appendl
