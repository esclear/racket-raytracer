#lang racket
(provide (all-defined-out))

(require "geometry.rkt" "objects.rkt" "helpers.rkt")

(struct camera (position
                ; These vectors must be normalized
                direction camright camup
                ; The width and height in pixels of the
                ; image being rendered
                width height))

(struct light (direction color intensity))

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
                                        (- 1 (/ (* 2 y) height))))))))

; Intersect the given ray with the scene
(define (intersect-ray ray scene)
  (let* ( (objects (scene-objects scene))
          (ray-intersections (map (lambda (obj) (send obj intersect ray)) objects)) )
    (filter (lambda (x) (and x (positive? (intersection-distance x)))) ray-intersections)))

; Trace the given ray
(define (trace-ray tray scene)
  (let* ( (intersections (intersect-ray tray scene)) )
    (if (not (null? intersections))
        (let* ( (ray-intersection (smallest intersection-distance intersections))
                (iobject (intersection-object ray-intersection))
                (inormal (intersection-normal ray-intersection))
                ; We add some tiny vector in the direction of the normal in order not to have black pixels on lit surfaces, because due to
                ; the inexact nature of floats the coordinates could be a little bit off and the lit surface cast a shadow on itself.
                (ipoint  (vec->point (vec-add (intersection-point ray-intersection)
                                              (vec-scale inormal 1E-13))))

                (lights (scene-lights scene)) )
          (apply color-mix (map (lambda (light)
            ; Here we use lambert's cosine law, as explained in
            ; https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/diffuse-lambertian-shading
            (let* ( (to-light-direction (vec-reverse (light-direction light)))
                    (shadow-ray (ray ipoint to-light-direction))
                    (power (* (vec-dot inormal to-light-direction) (light-intensity light)))
                    (reflected (/ (material-reflectivity (send iobject get-material)) pi))
                    (color (material-color (send iobject get-material)))
                    (lightyness (if (not (null? (intersect-ray shadow-ray scene))) 0 1)) )

              (color-scale (* power reflected lightyness)
                           (color-mul color (light-color light)))
              ))
          lights)))
        #f
        )))