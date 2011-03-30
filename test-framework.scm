#lang scheme
(provide test)
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
