#lang racket

(require racket/draw "graphics.rkt" "geometry.rkt" "helpers.rkt" "objects.rkt")

; hehe, that name :)
(define (make-scene)
  (let ((red    (material (color 1 0 0) 1))
        (green  (material (color 0 1 0) 1))
        (blue   (material (color 0 0 1) 1.75))
        (yellow (material (color 1 1 0) 1))
        (grey   (material (color 0.5 0.5 0.5) 1.5))
        (white  (material (color 1 1 1) 2)))
    (scene (setup-camera)
           (list (new sphere% [center (point 1.5 1 1.5)]   [radius 1]   [material grey])
                 (new sphere% [center (point -1 -0.5 3.2)] [radius 1.2] [material blue])

                 ;(new plane% [point (point 0 0 0)] [normal (vec 0 1 0)] [material white])
                 (new plane% [point (point 0 0 5)] [normal (vec 0 0 1)] [material white])
                 ;(new plane% [point (point 0 5 0)] [normal (vec 0 1 0)] [material white])

                 ;(new plane% [point (point 0 0 2)] [normal (vec 0 0 1)] [material yellow])

                 ;(new plane% [point (point 3 1 0)]  [normal (vec 1 0 0)] [material red])
                 (new plane% [point (point -3 1 0)] [normal (vec 1 0 0)] [material green])
                 )
           (list (light (vec 1 2 0.5) (color 1 1 1) 2.5)))))

(define (setup-camera)
  (camera (origin)
          ; Vectors describing the rotation of the camera,
          ; to be produced by a custom translation matrix
          ; or simmilar.
          (vec 0 0 1) (vec (/ 4 3) 0 0) (vec 0 1 0)
          ; The glorious 800x600 pixel resolution
          800 600))

; Render the a scene produced by (make-scene), producing a bitmap and drawing
; the result of a raytrace for each pixel
(define (render)
  (let* ((scene (make-scene))
         (cam   (scene-camera scene))
         (image (make-bitmap (camera-width cam) (camera-height cam)))
         (bdc (new bitmap-dc% [bitmap image])))

    (for ([y (range (camera-height cam))])
      (for ([x (range (camera-width cam))])
        (let* ((ray         (cast-primary-ray cam x y))
               (pixel-color (trace-ray ray scene)))
          (if pixel-color
              (begin
                (send bdc set-brush (color->drawingcolor pixel-color) 'solid)
                (send bdc draw-rectangle x y 1 1))
              'nop))))
    (displayln "Done, look at that great rendering:")
    image))

(time (render))
