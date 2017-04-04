#lang racket

(require racket/draw "graphics.rkt" "geometry.rkt" "objects.rkt")

; hehe, that name :)
(define (make-scene)
  (let ((red   (material (make-object color% 255 0 0)))
        (green (material (make-object color% 0 255 0)))
        (blue  (material (make-object color% 0 0 255)))
        (grey  (material (make-object color% 127 127 127))))
  (list (sphere (point 0 1 2) 1 red)
        (plane (point 0 0 0) (vec 0 1 0) grey)
        )))

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
        (let ((intersection (intersects (cast-primary-ray cam x y) sphere)))
          (if intersection
              (begin
                (send bdc set-brush (material-color (cdr intersection)) 'solid)
                (send bdc draw-rectangle x y 1 1))
              'nop)
          )))
    (displayln "Done, look at that great rendering:")
    image
    ))

(time (render))