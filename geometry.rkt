#lang typed/racket
(require racket/struct)

(provide (all-defined-out))

;; POINT IN 3D-SPACE
(struct point ([x : Number] [y : Number] [z : Number])
  #:transparent)
(: origin (-> point))
(define (origin)
  (point 0 0 0))

;; VECTOR IN 3D-SPACE
(struct ray ([point : point] [direction : vec])
  #:transparent)

;; THREE-DIMENSIONAL VECTOR
(struct vec ([x : Number] [y : Number] [z : Number])
  #:transparent)

(: zerovec (-> vec))
(define (zerovec)
  (vec 0 0 0))

; Get the length of a given vector v
(: vec-len (-> vec Number))
(define (vec-len v)
  (let ((a (vec-x v))
        (b (vec-y v))
        (c (vec-z v)))
    (sqrt (+ (* a a) (* b b) (* c c)))))

; Add multiple vectors
(: vec-add (-> vec * vec))
(define (vec-add . vecs)
  (vec (foldl + 0 (map vec-x vecs))
       (foldl + 0 (map vec-y vecs))
       (foldl + 0 (map vec-z vecs))))

; Subtract the vectors in vecs from the vector a
(: vec-sub (-> vec vec * vec))
(define (vec-sub a . vecs)
  (vec-add a (vec-reverse (apply vec-add vecs))))

; Normalize a given vector v so that
; (vec-len (vec-normalize v)) = 1
; There is but one exception, as the vector (0 0 0)
; cannot be normalized in the sense that the result
; has the same "direction" as v, thus (0 0 0) is returned
; in this case.
(: vec-normalize (-> vec vec))
(define (vec-normalize v)
  (let ((len (vec-len v)))
    (if (zero? len)
        (zerovec)
        (vec-scale v (/ len)))))

; Scale a vector v by the given factor
(: vec-scale (-> vec Number vec))
(define (vec-scale v factor)
  (vec (* (vec-x v) factor)
       (* (vec-y v) factor)
       (* (vec-z v) factor)))

; Reverse a given vector v
(: vec-reverse (-> vec vec))
(define (vec-reverse v)
  (vec-scale v (- 1)))

; Calculate the dot-product of two vectors a and b
(: vec-dot (-> vec vec Number))
(define (vec-dot a b)
  (+ (* (vec-x a) (vec-x b))
     (* (vec-y a) (vec-y b))
     (* (vec-z a) (vec-z b))))

; Calculate the cross-product of two vectors a and b
(: vec-cross (-> vec vec vec))
(define (vec-cross a b)
  (let ((xa (vec-x a)) (xb (vec-x b))
                       (ya (vec-y a)) (yb (vec-y b))
                       (za (vec-z a)) (zb (vec-z b)))
    (vec (- (* ya zb) (* za yb))
         (- (* za xb) (* xa zb))
         (- (* xa yb) (* ya xb)))))

; Convert a point to a vector
(: point->vec (-> point vec))
(define (point->vec point)
  (vec (point-x point)
       (point-y point)
       (point-z point)))

; Convert a vector to a point
(: vec->point (-> vec point))
(define (vec->point vec)
  (point (vec-x vec)
         (vec-y vec)
         (vec-z vec)))