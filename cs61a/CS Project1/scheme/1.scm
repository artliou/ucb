;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: CS Est. 61a
;;;
;;; Description:
;;;    The many layers
;;;    Of the 61a life!
;;;    Oh 'treely' vibrant

(define (draw)
  ; *YOUR CODE HERE*
  (goto -10 245)
  (speed 0)
  (tracer 150)
  (bgcolor 'black)
  (rgb '255 '0 '0)
  (star 20 5)

  (penup)
  (setposition 0 -200)
  (setheading 90)
  (pendown)
  (tree 250)

  (penup)
  (setposition 65 -120)
  (pendown)
  (rgb '255 '165 '0)
  (write 'cs '120  'left)
  (penup)
  (setposition 0 -250)
  (pendown)
  (rgb '253 '227 '0)
  (write '(est 61) '18 'center)
  (penup)
  (setheading 0)
  (fd 25)
  (pendown)
  (write 'a '18 'center)
  (exitonclick))
  
(define (repeat k fn)
  (if (> k 0)
      (begin (fn) (changergb) (repeat (- k 1) fn))
      'done))

(define (star len angle)
	(repeat 72 (lambda () (repeat 5 (lambda () (fd 10) (rt 144) (fd 10) (lt 72)))
          (pu) (fd len) (pd) (rt angle))))

(define (tree r)
  (changergb)
  (if (< r 5) (begin (forward r) (back r))
  (begin 
    (forward (/ r 3))
    (left 30)
    (tree (* (/ r 3) 2))
    (right 30)
    (back (/ r 3))

    (forward (/ r 2))
    (right 25)
    (tree (/ r 2))
    (left 25)
    (back (/ r 2))

    (forward (* (/ 5 6) r))
    (right 25)
    (tree (/ r 2))
    (left 25)
    (back (* r (/ 5 6))))))

(hideturtle)
; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)

