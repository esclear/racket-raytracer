#lang racket
(provide (all-defined-out))

(define (smallest key list)
  (cond
    ((null? list) #f)
    ((null? (cdr list)) (car list))
    (else (let ((tail (smallest key (cdr list))))
          (if (<= (key (car list))
                  (key tail))
              (car list)
              tail)))))