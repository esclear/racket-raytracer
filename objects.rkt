#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct sphere (center radius))

(define (intersects ray sphere)
  (if (not (sphere? sphere))
      #f
      (let* ((rad (sphere-radius sphere))
             (sqr (* rad rad))
             
             (con (vec-sub (point->vec (sphere-center sphere))
                           (point->vec (ray-point ray))))
             (adj (vec-dot con (ray-direction ray)))
             (d2  (- (vec-dot con con) (* adj adj))))

        (if (< sqr d2) #f
            (let* ((ts (sqrt (- sqr d2)))
                   (ta (- adj ts))
                   (tb (+ adj ts)))
              (cond
                ((and (< ta 0) (< tb 0)) #f)
                ((< ta 0) tb)
                ((< tb 0) ta)
                (else (if (< ta tb) ta tb)
              )))))))
