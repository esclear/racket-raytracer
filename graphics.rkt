#lang racket
(provide (all-defined-out))

(require "geometry.rkt")

(struct camera (position direction width height fov))

(struct scene (camera objects))

; reflect a vec v at a surface with the given normal
; TODO: Implement this
(define (reflect v normal)
  (zerovec))