#lang racket

(require (rename-in graphics/turtles
           (split turtle-split)))

(provide (all-defined-out))

;; Code for integrating Racket's turtle graphics library

(define turtle-x -1)
(define turtle-y -1)
(define turtle-pen-down #f)

(define (cs)
  (turtles #t)
  (clear)
  (set! turtle-x (/ turtle-window-size 2))
  (set! turtle-y (/ turtle-window-size 2))
  (set! turtle-pen-down #f))

(define (penup)
  (set! turtle-pen-down #f))

(define (pendown)
  (set! turtle-pen-down #t))

(define (setxy x y)
  (let ((relative-x (- x turtle-x))
        (relative-y (* -1 (- y turtle-y))))
  (begin (if turtle-pen-down
             (draw-offset relative-x relative-y)
             (move-offset relative-x relative-y))
         (set! turtle-x x)
         (set! turtle-y y))))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v1) turtle-window-size) (/ turtle-window-size 2)))
  (pendown)
  (setxy (- (* (xcor-vect v2) turtle-window-size) (/ turtle-window-size 2))
         (- (* (ycor-vect v2) turtle-window-size) (/ turtle-window-size 2))))

(define (export filename)
  (save-turtle-bitmap (string->path filename) 'png))

;; Code for the picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
  (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
      (right (right-split painter (- n 1))))
  (let ((top-left (beside up up))
        (bottom-right (below right right))
        (corner (corner-split painter (- n 1))))
    (beside (below painter top-left)
      (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
    (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
         (edge1-frame frame))
         (scale-vect (ycor-vect v)
         (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
  ((frame-coord-map frame) (start-segment segment))
  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
  (painter
   (make-frame new-origin
         (sub-vect (m corner1) new-origin)
         (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
         (make-vect 0.0 1.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
        (make-vect 0.5 0.5)
        (make-vect 1.0 0.5)
        (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
         (make-vect 1.0 0.0)
         (make-vect 1.0 1.0)
         (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
         (make-vect 0.0 0.0)
         (make-vect 0.65 0.35)
         (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
     (transform-painter painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
    (paint-right
     (transform-painter painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
  (paint-left frame)
  (paint-right frame)))))

;; End of picture language code


;;
;; Your code goes below
;;

;; Exercise 1

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Exercise 2

(define (split major minor painter n)
  (if (equal? major beside)
    (right-split painter n)
    (up-split painter n)
    )
)
;Added the painter n. If major is besides, then use right-split

;For Exercise 2, you'll have to decide what the two arguments mean. 
;Remember that split is a general form of right-split and up-split. 
;It's like how accumulate is a general form of sum and product (from HW 2).

;; Exercise 3

(define (make-vect major minor)
  (list major minor))

(define xcor-vect
  car)

(define ycor-vect
  cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v)))
)

;; Execise 4

; First definition of make-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame
  car)

(define edge1-frame
  cadr)

(define edge2-frame
  caddr)

; Second definition of make-frame

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-2
  car)

(define edge1-frame-2
  cadr)

(define edge2-frame-2
  caddr)

;; Exercise 5
;A directed line segment in the plane can be represented as a pair of vectors—the vector 
;running from the origin to the start-point of the segment, 
;and the vector running from the origin to the end-point of the segment. 
;Use your vector representation from above to define a representation for 
;segments with constructor make-segment and selectors start-segment and end-segment.

(define (make-segment v1 v2)
  (list v1 v2))
;Should take in two arguments

(define start-segment
  car)

(define end-segment
  cadr)

;; Exercise 6

(define outline-painter
  (segments->painter
  (list (make-segment (make-vect 0 0) (make-vect 0 1))
        (make-segment (make-vect 0 1) (make-vect 1 1))
        (make-segment (make-vect 1 1) (make-vect 1 0))
        (make-segment (make-vect 1 0) (make-vect 0 0))
)))

(define x-painter
  (segments->painter
  (list (make-segment (make-vect 1 0) (make-vect 0 1))
        (make-segment (make-vect 0 0) (make-vect 1 1))
)))

(define diamond-painter
  (segments->painter
  (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
        (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
        (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
        (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
)))

(define wave-painter
  (segments->painter
  (list (make-segment (make-vect 0 0.8) (make-vect 0.2 0.6))
        (make-segment (make-vect 0.2 0.6) (make-vect 0.35 0.65))
        (make-segment (make-vect 0.35 0.65) (make-vect 0.4 0.65))
        (make-segment (make-vect 0.4 0.65) (make-vect 0.37 0.8))
        (make-segment (make-vect 0.37 0.8) (make-vect 0.4 1))
        (make-segment (make-vect 0.6 1) (make-vect 0.63 0.8))
        (make-segment (make-vect 0.63 0.8) (make-vect 0.6 0.65))
        (make-segment (make-vect 0.6 0.65) (make-vect 0.8 0.65))
        (make-segment (make-vect 0.8 0.6) (make-vect 1 0.3))
        (make-segment (make-vect 1 0.1) (make-vect 0.6 0.4))
        (make-segment (make-vect 0.6 0.4) (make-vect 0.7 0))
        (make-segment (make-vect 0.55 0) (make-vect 0.5 0.25))
        (make-segment (make-vect 0.5 0.25) (make-vect 0.45 0))
        (make-segment (make-vect 0.3 0) (make-vect 0.35 0.5))
        (make-segment (make-vect 0.35 0.5) (make-vect 0.32 0.55))
        (make-segment (make-vect 0.32 0.55) (make-vect 0.2 0.4))
        (make-segment (make-vect 0.2 0.4) (make-vect 0 0.6))

        ;add smile
        (make-segment (make-vect 0.35 0.60) (make-vect 0.55 0.6))
        (make-segment (make-vect 0.55 0.6) (make-vect 0.75 0.7))
        (make-segment (make-vect 0.2 0.7) (make-vect 0.35 0.60))   
)))

;; Exercise 7

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect -1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)))

;; Exercise 8

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.5 0.5)))
    (let ((paint-up
     (transform-painter painter1
            (make-vect 0.5 0.5)
            split-point
            (make-vect 0.5 1.0)))
    (paint-down
     (transform-painter painter2
            split-point
            (make-vect 0.5 0.5)
            (make-vect 0.5 0))))
      (lambda (frame)
  (paint-up frame)
  (paint-down frame)))))
;DRAW THE PAINTER

(define (below-2 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.5)))
  (identity painter1)
  (rotate180 painter2)))

  ;(error "not yet implemented"))

;; Exercise 9

; Modify wave-painter above (Exercise 6)

; Modify me!
(define (corner-split-2 painter n)
  (if (= n 0)
      painter
      (let ((up (right-split painter (- n 1)))
      (right (up-split painter (- n 1))))
  (let ((top-left (below right right))
        (bottom-right (beside up up))
        (corner (corner-split-2 painter (- n 1))))
    (beside (below painter top-left)
      (below bottom-right corner))))))
;How modified: switched the top-left and bottom-right; Then up>right-split and right>upsplit

; Modify me!
(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four rotate180 flip-vert 
                                  flip-horiz identity)))
    (combine4 (corner-split painter n))))
;I switched the rotate180/flip-vert with flip-horz/identity!!!!

;; End of project
;; Don't touch anything below this

(define full-frame
  (make-frame (make-vect 0.5 0.5)
              (make-vect 1 0)
              (make-vect 0 1)))