#lang racket

(require  test-engine/racket-tests "../geometry.rkt" "../graphics.rkt")

(check-expect (zerovec) (vec 0 0 0))
(check-expect (vec-len (zerovec)) 0)
(check-expect (vec-reverse (vec 1 2 3)) (vec -1 -2 -3))

(check-expect (vec-len (vec 1 0 0)) 1)
(check-within (vec-len (vec 1 2 3)) (vec-len (vec 3 1 2)) 1E-6)

(check-within (vec-len (vec-normalize (vec 1 2 3))) 1 1E-6)

(check-expect (vec-cross (vec 1 0 0) (vec 0 1 0)) (vec 0 0 1))

(check-expect (vec-dot (vec 1 2 3) (zerovec)) 0)
(check-expect (vec-dot (vec 1 2 3) (vec 4 5 6)) 32)

(check-expect (vec-add) (zerovec))
(check-expect (vec-add (zerovec)) (zerovec))
(check-expect (vec-add (vec 1 2 3) (zerovec)) (vec 1 2 3))

(check-random (begin (vec (random 50) (random 50) (random 50)))
              (begin (vec (random 50) (random 50) (random 50))))

(check-expect (vec-sub (vec 4 5 6) (vec 1 2 3) (zerovec)) (vec 3 3 3))

(check-expect (vec-scale (vec 1 2 3) 2) (vec 2 4 6))

; Vector reflection

(check-expect (vec-reflect (vec 1 -1 0) (vec 0 1 0)) (vec 1 1 0))
(check-expect (vec-reflect (vec 1 1 -1) (vec 0 0 1)) (vec 1 1 1))
(check-expect (vec-reflect (vec 0 0 -1) (vec 0 0 1)) (vec 0 0 1))

; Point projection
(check-expect (pora-project (origin) (ray (origin) (vec 1 2 3))) (origin))
(check-expect (pora-project (point 4 1 2) (ray (origin) (vec 1 0 0))) (point 4 0 0))
(check-expect (pora-project (point 4 1 2) (ray (point 2 0 0) (vec 1 0 0))) (point 4 0 0))
(check-expect (pora-project (point 4 1 2) (ray (point 2 1 0) (vec 1 0 0))) (point 4 1 0))
