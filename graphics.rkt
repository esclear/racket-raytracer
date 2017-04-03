#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct camera (position
                ; These vectors must be normalized
                direction camright camup
                ; The width and height in pixels of the
                ; image being rendered
                width height))

(struct scene (camera objects))

; reflect a vec v at a surface with the given normal
; TODO: Implement this
(define (reflect v normal)
  (zerovec))

(define (cast-primary-ray cam x y)
  (let ((width     (camera-width cam))
        (height    (camera-height cam))

        (origin    (camera-position cam))

        (centervec (camera-direction cam))
        (upvec     (camera-camup cam))
        (rightvec  (camera-camright cam)))

    (ray
     origin
     (vec-normalize (vec-add centervec
                             (vec-scale rightvec
                                        (- (/ (* 2 x) width ) 1))
                             (vec-scale upvec
                                        (- (/ (* 2 y) height) 1)))))))