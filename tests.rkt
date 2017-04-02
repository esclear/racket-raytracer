#lang racket

(require test-engine/racket-tests)

(define (approx-eq a b)
  (approx-eq-e a b 1E-5))

(define (approx-eq-e a b epsilon)
  (< (abs (- a b)) epsilon))