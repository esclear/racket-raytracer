#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct material (color reflectivity))

;
; RAYTRACABLE OBJECTS (CALLED ENTITIES HERE)
;
(define raytrace-interface (interface ()
                             intersect intersection-normal))

(define renderable%
  (class object%
    (init material)

    (define current-material material)

    (super-new)

    (define/public (get-material)
      current-material)

    (define/public (set-material mat)
      (set! current-material mat))))

; A sphere with a given center point and radius
(define sphere%
  (class* renderable% (raytrace-interface)

    (init center radius)

    (define current-center center)
    (define current-radius radius)

    (super-new)

    (define/public (get-center)
      current-center)
    (define/public (get-radius)
      current-radius)

    (define/public (set-center center)
      (set! current-center center))
    (define/public (set-radius radius)
      (set! current-radius radius))

    (define/public (intersect ray)
      (let* ((sqr (* current-radius current-radius))
             
             (con (vec-sub (point->vec current-center)
                           (point->vec (ray-point ray))))
             (adj (vec-dot con (ray-direction ray)))
             (d2  (- (vec-dot con con) (* adj adj))))

        (and (< d2 sqr)
             (cons
              (let* ((ts (sqrt (- sqr d2)))
                     (ta (- adj ts))
                     (tb (+ adj ts)))
                (cond
                  ((and (< ta 0) (< tb 0)) #f)
                  ((< ta 0) tb)
                  ((< tb 0) ta)
                  (else (if (< ta tb) ta tb)
                        )))
              (send this get-material)))))

    (define/public (intersection-normal point)
      (vec-normalize (vec-sub (point->vec point)
                              (point->vec current-center))))))


; A plane define by a point on that plane and a normal
(define plane%
  (class* renderable% (raytrace-interface)

    (init point normal)

    (define current-point point)
    (define current-normal normal)

    (super-new)

    (define/public (get-point)
      current-point)
    (define/public (get-normal)
      current-normal)

    (define/public (set-point point)
      (set! current-point point))
    (define/public (set-normal normal)
      (set! current-normal normal))

    (define/public (intersect ray)
      (let ((ppoint (point->vec current-point))
            (rdirec (ray-direction ray))
            (rpoint (point->vec (ray-point ray))))
        (and (not (zero? (vec-dot current-normal rdirec)))
             (cons (/ (vec-dot (vec-sub ppoint rpoint) current-normal)
                      (vec-dot rdirec current-normal))
                   (send this get-material)))))

    (define/public (intersection-normal point)
      (vec-normalize current-normal))))

;(struct triagle (one two three))