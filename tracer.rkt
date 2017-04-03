#lang racket

(require racket/draw "graphics.rkt" "geometry.rkt")

; hehe, that name :)
(define (make-scene)
  'tbd
  )
(define (setup-camera)
  (camera (origin)
          ; Vectors describing the rotation of the camera,
          ; to be produced by a custom translation matrix
          ; or simmilar.
          (vec 0 0 -1) (vec 4 0 0) (vec 3 1 0)
          ; The glorious 800x600 pixel resolution
          800 600))

(define (render)
  (letrec ((cam   (setup-camera))
           (scene (make-scene))
           (image (make-bitmap (camera-width cam) (camera-height cam))))
    (for ([x (range (camera-width cam))])
      ;(printf "Rendered column ~s of ~s~n" x (camera-width cam))
      (for ([y (range (camera-height cam))])
        'tbd
        ))
    (displayln "Done, look at that great rendering:")
    image
    ))

(time (render))