#lang racket

(require racket/draw "graphics.rkt" "geometry.rkt" "objects.rkt" "helpers.rkt")

; hehe, that name :)
(define (make-scene)
  (let ((red   (material (make-object color% 255 0 0)))
        (green (material (make-object color% 0 255 0)))
        (blue  (material (make-object color% 0 0 255)))
        (grey  (material (make-object color% 127 127 127)))
        (white (material (make-object color% 255 255 255))))
  (list (sphere (point 0 1 1.5) 1 grey)
        (sphere (point 1 0.75 3.2) 1.2 blue)

        (plane (point 0 0 0) (vec 0 1 0) white)
        (plane (point 0 0 5) (vec 0 0 1) white)
        (plane (point 0 5 0) (vec 0 1 0) white)

        (plane (point 3 1 0) (vec 1 0 0) red)
        (plane (point -3 1 0) (vec 1 0 0) green)
        )))

(define (setup-camera)
  (camera (origin)
          ; Vectors describing the rotation of the camera,
          ; to be produced by a custom translation matrix
          ; or simmilar.
          (vec 0 0 1) (vec (/ 4 3) 0 0) (vec 0 1 0)
          ; The glorious 800x600 pixel resolution
          800 600))

(define (render)
  (let* ((cam   (setup-camera))
         (scene (make-scene))
         (sphere (car scene))
         (image (make-bitmap (camera-width cam) (camera-height cam)))
         (bdc (new bitmap-dc% [bitmap image])))
    (for ([y (range (camera-height cam))])
      (for ([x (range (camera-width cam))])
        (let* ((ray (cast-primary-ray cam x y))
               (intersections (filter (lambda (x) (and (not (not x)) (positive? (car x))))
                                      (map (lambda (obj) (intersects ray obj)) scene))))
          (if (and intersections (not (null? intersections)) (pair? (car intersections)))
              (let ((intersection (cdr (smallest car intersections))))
                (send bdc set-brush (material-color intersection) 'solid)
                (send bdc draw-rectangle x y 1 1))
              'nop)
          )))
    (displayln "Done, look at that great rendering:")
    image
    ))

(time (render))