#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct material (color reflectivity)
  #:transparent)
(struct intersection (distance point normal object)
  #:transparent)

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
      (let* ( (vcenter (point->vec current-center))
              (radius  current-radius)
              (rpoint  (point->vec (ray-point ray)))
              (rdir    (ray-direction ray))
              (vpc     (vec-sub vcenter rpoint)) )
        (if (negative? (vec-dot vpc rdir))
            (and (<= (vec-len vpc) radius)
                 (if (= (vec-len vpc) radius)
                     (intersection 0 rpoint (intersection-normal rpoint) this)
                     (let* ( (pc  (point->vec (pora-project current-center ray)))
                             (pcl (vec-len (vec-sub pc vcenter)))
                             (locdist  (sqrt (- (* radius radius)
                                                (* pcl pcl))))
                             (distance (- locdist (vec-len (vec-sub pc rpoint))))
                             (point (vec-add (point->vec (ray-point ray))
                                             (vec-scale (ray-direction ray) distance))) )
                       (intersection distance
                                     point
                                     (intersection-normal point)
                                     this))))
            (let* ( (pc  (point->vec (pora-project current-center ray)))
                    (pcl (vec-len (vec-sub pc vcenter))) )
              (and (<= pcl radius)
                   (let* ( (locdist  (sqrt (- (* radius radius)
                                              (* pcl pcl))))
                           (distance (if (> (vec-len vpc) radius)
                                         (- (vec-len (vec-sub pc rpoint)) locdist)
                                         (+ (vec-len (vec-sub pc rpoint)) locdist)))
                           (point (vec-add (point->vec (ray-point ray))
                                           (vec-scale (ray-direction ray) distance))) )
                     (intersection distance
                                   point
                                   (intersection-normal point)
                                   this)))))))

    (define/public (intersection-normal point)
      (vec-normalize (vec-sub point
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
             (let* ((distance (/ (vec-dot (vec-sub ppoint rpoint) current-normal)
                                 (vec-dot rdirec current-normal)))
                    (point (vec-add (point->vec (ray-point ray))
                                    (vec-scale (ray-direction ray) distance))))
               (intersection distance
                             point
                             (intersection-normal point)
                             this)))))

    (define/public (intersection-normal point)
      (vec-normalize current-normal))))

; A triangle defined by its three corners
(define triangle%
  (class* renderable% (raytrace-interface)

    (init a b c)

    (define current-a a)
    (define current-b b)
    (define current-c c)

    (super-new)

    (define/public (get-a) current-a)
    (define/public (get-b) current-b)
    (define/public (get-c) current-c)

    (define/public (set-a a)
      (set! current-a a))
    (define/public (set-b b)
      (set! current-b b))
    (define/public (set-c c)
      (set! current-c c))

    ; Behold!
    ; This is an implementation of the Möller-Trumbone algorithm.
    ; It looks a little bit strange, but it was more or less optimized
    ; for speed, as much as it was possible in a trial-and-error fashion
    ; in racket.
    (define/public (intersect ray)
      (let* ( (rdir  (ray-direction ray))
              (edge1 (vec-sub (point->vec current-b) (point->vec current-a)))
              (edge2 (vec-sub (point->vec current-c) (point->vec current-a)))
              (pvec  (vec-cross rdir edge2))
              (det   (vec-dot edge1 pvec)) )
        (and (not (< (abs det) 1E-12))
             (let* ( (inv-det (/ det))
                     (tvec    (vec-sub (point->vec (ray-point ray))
                                       (point->vec current-a)))
                     (u       (* inv-det (vec-dot tvec pvec))) )
               (and (not (or (< u 0) (> u 1)))
                    (let* ( (qvec (vec-cross tvec edge1))
                            (v    (* inv-det (vec-dot rdir qvec))) )
                      (and (not (or (< v 0) (> (+ u v) 1)))
                           (let* ( (distance  (* inv-det (vec-dot edge2 qvec)))
                                   (point (vec-add (point->vec (ray-point ray))
                                                   (vec-scale (ray-direction ray) distance))) )
                             (intersection distance
                                           point
                                           (intersection-normal point)
                                           this)))))
               ))))

    (define/public (intersection-normal point)
      (vec-normalize (vec-cross (vec-sub (point->vec current-c) (point->vec current-a))
                                (vec-sub (point->vec current-b) (point->vec current-a)))))))