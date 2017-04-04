#lang racket

(require  test-engine/racket-tests "../helpers.rkt")

(define self (lambda (x) x))

(check-expect (smallest self '()) #f)
(check-expect (smallest self '(1)) 1)
(check-expect (smallest self '(1 2)) 1)

(check-expect (smallest car '((4 'a) (2 'b) (3 'c))) '(2 'b))