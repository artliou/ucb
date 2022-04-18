;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: The Circle of Colors
;;;
;;; Description:
;;;   What Goes around Comes around !?!

(define (draw)
  (define (cr size times n)
  (cond ((eq? times 0))
        (else  (right (/ 360 n)) (circle size) (cr size (- times 1) n ))))

    (speed 0)
    (define rainbow '(red orange yellow green blue purple violet))
    (define loop 140)
    (define rad 200)
    (define (split_cr rainbow)
        (cond ((null? rainbow))
              (else (color (car rainbow)) (cr rad (/ loop 7) loop) (split_cr (cdr rainbow)))))  
    
    (split_cr rainbow)
    (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)
