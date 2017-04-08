#lang racket
(provide (all-defined-out))

(require racket/draw)

(define (smallest key list)
  (cond
    ((null? list) #f)
    ((null? (cdr list)) (car list))
    (else (let ((tail (smallest key (cdr list))))
            (if (<= (key (car list))
                    (key tail))
                (car list)
                tail)))))

; Color stuff
(struct color (red green blue)
  #:transparent)

(define (color-scale factor input)
  (color (* factor (color-red   input))
         (* factor (color-green input))
         (* factor (color-blue  input))))

(define (color-clamp input)
  (color (min 1 (max 0 (color-red   input)))
         (min 1 (max 0 (color-green input)))
         (min 1 (max 0 (color-blue  input)))))

(define (color-mul . colors)
  (color (foldl * 1 (map color-red   colors))
         (foldl * 1 (map color-green colors))
         (foldl * 1 (map color-blue  colors))))

(define (color->drawingcolor col)
  (let ((clamped-color (color-clamp col)))
    (make-object color%
      (exact-round (* 255 (color-red   clamped-color)))
      (exact-round (* 255 (color-green clamped-color)))
      (exact-round (* 255 (color-blue  clamped-color))))))

(define (color-add . colors)
  (color (foldl + 0 (map color-red   colors))
         (foldl + 0 (map color-green colors))
         (foldl + 0 (map color-blue  colors))))

(define (color-mix . colors)
  (color-scale (/ (length colors)) (apply color-add colors)))