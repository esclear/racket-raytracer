#lang racket

(require racket/draw "data-structures.rkt" "geometry.rkt")

; hehe, that name :)
(define (make-scene)
  'tbd
  )
(define (setup-camera)
  ; glorious 800 x 600 resolution
  (camera (origin) (zerovec) 800 600 90)
  )

(define (render)
  (letrec ((cam   (setup-camera))
           (scene (make-scene))
           (image (make-bitmap (camera-width cam) (camera-height cam))))
    (for ([x (range (camera-width cam))])
      (printf "Rendered column ~s of ~s~n" x (camera-width cam))
      (for ([y (range (camera-height cam))])
        'tbd
        ))
    (displayln "Done, look at that great rendering:")
    image
    ))

(render)