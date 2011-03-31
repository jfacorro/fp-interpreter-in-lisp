#lang scheme
(require "fp-functions.scm")

(define (map-symbol-to-function symbol)
  (cond ((equal? symbol 'id) 'fp-id)
        ((equal? symbol 'tl) 'fp-tl)
        ((equal? symbol 'tlr) 'fp-tlr)
        ((equal? symbol 'atom) 'fp-atom)
        ((equal? symbol 'eq) 'fp-eq)
        ((equal? symbol 'null) 'fp-null)
        ((equal? symbol 'reverse) 'fp-reverse)
        ((equal? symbol 'iota) 'fp-iota)
        ((equal? symbol 'distl) 'fp-distl)
        ((equal? symbol 'distr) 'fp-distr)
        ((equal? symbol 'length) 'fp-length)
        ((equal? symbol '-) 'fp--)
        ((equal? symbol '+) 'fp-+)
        ((equal? symbol '*) 'fp-*)
        ((equal? symbol '%) 'fp-%)
        ((equal? symbol '<) 'fp-<)
        ((equal? symbol '>) 'fp->)
        ((equal? symbol 'trans) 'fp-trans)
        ((equal? symbol 'and) 'fp-and)
        ((equal? symbol 'or) 'fp-or)
        ((equal? symbol 'not) 'fp-not)
        ((equal? symbol 'appendl) 'fp-appendl)
        ((equal? symbol 'rot) 'fp-rot)
        ((equal? symbol 'rotr) 'fp-rotr)
        ((equal? symbol 'o) 'fp-compose)
        ((equal? symbol '[]) 'fp-construct)
        ((equal? symbol '~) 'fp-const)
        ((equal? symbol '->) 'fp-cond)
        ((equal? symbol '/) 'fp-insert)
        ((equal? symbol 'alpha) 'fp-alpha)
        (else null)))

(define (fp-interpret code)
   code)

(define (tokenize str)
  (string-split str " "))
;----------------------------------------------
; string-split
;----------------------------------------------
(define (string-split str delim)
  (let ((index (string-first-index str delim)))
    (cond ((>= index 0) (append (list (substring str 0 index)) (string-split (substring str (+ index 1)) delim)))
          (else 
           (if (string=? "" str) null (list str))))))
;----------------------------------------------
; string-first-index
;----------------------------------------------
(define (string-first-index str char)
  (string-first-index-helper str char 0))

(define (string-first-index-helper str char index)
  (cond ((string=? str "") -1)
        (else
         (if (equal? char (string-ref str 0))
             index 
             (string-first-index-helper (substring str 1) char (+ 1 index))))))
;----------------------------------------------

(tokenize "Juan Martin")
(string-split "id o 1" #\ )
(map-symbol-to-function '/)
(fp-interpret '(id o 1))

