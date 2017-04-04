#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct material (color))

(struct sphere (center radius material))

(struct plane (point normal material))


(define (intersects ray object)
  (cond
    ((sphere? object)
     (let* ((rad (sphere-radius object))
            (sqr (* rad rad))
             
            (con (vec-sub (point->vec (sphere-center object))
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
             (sphere-material object)))))
    ((plane? object)
     (let ((normal (plane-normal object))
           (ppoint (plane-point object))
           (rdirec (ray-direction ray))
           (rpoint (ray-point ray)))
       (and (not (zero? (vec-dot normal rdirec)))
            (cons (/ (vec-dot (vec-sub ppoint rpoint) normal)
                     (vec-dot rdirec normal))
                  (plane-material object)))))
    (else #f)))
