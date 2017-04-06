#lang racket
(provide (all-defined-out))

(require "geometry.rkt" "helpers.rkt")

(struct camera (position
                ; These vectors must be normalized
                direction camright camup
                ; The width and height in pixels of the
                ; image being rendered
                width height))

(struct light (direction))

(struct scene (camera objects lights))

; reflect the vector vec at a surface with the given
; normal
(define (vec-reflect vec normal)
  (let ((normalized-normal (vec-normalize normal)))
  (vec-sub vec
           (vec-scale normalized-normal (* 2 (vec-dot vec normalized-normal))))))

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

; Trace the given ray
(define (trace-ray ray scene)
  (let* ( (objects (scene-objects scene))
          (ray-intersections (map (lambda (obj) (send obj intersect ray)) objects))
          (intersections (filter (lambda (x) (and (not (not x)) (positive? (car x)))) ray-intersections)) )
    (if (and intersections (not (null? intersections)) (pair? (car intersections)))
        (cdr (smallest car intersections))
        #f
    )
  ))