#lang racket

(require racket/draw "graphics.rkt" "geometry.rkt" "objects.rkt")

; hehe, that name :)
(define (make-scene)
  (list (sphere (point 0 0 2) 1)
        ))

(define (setup-camera)
  (camera (origin)
          ; Vectors describing the rotation of the camera,
          ; to be produced by a custom translation matrix
          ; or simmilar.
          (vec 0 0 1) (vec 4 0 0) (vec 0 3 0)
          ; The glorious 800x600 pixel resolution
          800 600))

(define (render)
  (let* ((cam   (setup-camera))
         (scene (make-scene))
         (sphere (car scene))
         (image (make-bitmap (camera-width cam) (camera-height cam)))
         (bdc (new bitmap-dc% [bitmap image])))
    (for ([x (range (camera-width cam))])
      (for ([y (range (camera-height cam))])
        (begin
          (if (intersects (cast-primary-ray cam x y) sphere)
              (send bdc set-brush "black" 'solid)
              (send bdc set-brush "white" 'solid))
          (send bdc draw-rectangle x y 1 1)
          )))
    (displayln "Done, look at that great rendering:")
    image
    ))

(time (render))