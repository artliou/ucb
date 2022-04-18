;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Gang-nyan style.
;;;
;;; Description:
;;;    nyan nyan nyan nyan nyan
;;;    nyan cat doing gangnam style
;;;    john denero cat

(define (draw)
	(speed 0)
	(make-background)
	(color 'white)
	(star-grid -800 800 800 star)
	(star-grid -750 750 750 small-star)

	;Psy Cat
	(pensize 5)
	(move 300 150)
	(lt 90)
	(make-rainbow 13)
	(backward 150)
	(rt 90)
	(nyan)

	;Denero Cat
	(move 0 -125)
	(setheading 270)
	(make-rainbow 8)
	(backward 100)
	(setheading 0)
	(nyandenero)

  (exitonclick))
 ;(37) tokens
  
;Allows for increased token efficiency when used more than 8 times
(define (line dist ang) (fd dist) (lt ang))
;8 tokens

;Allows for increased token efficiency when used more than 5 times
(define (move ex why) (pu) (goto ex why) (pd))
;9 tokens
  
;Traces a single partition of the rainbow in the forward direction
(define (curve) 
			   (circle 90 30)
			   (circle 60 30)
			   (fd 30)
			   (circle -60 30)
			   (circle -90 60)
			   (circle -60 30)
			   (fd 30)
			   (circle 60 30)
			   (circle 90 30))
;21 tokens

;Traces a single partition of the rainbow in the reverse direction			   
(define (rev-curve)
			   (circle -90 30)
			   (circle -60 30)
			   (fd 30)
			   (circle 60 30)
			   (circle 90 60)
			   (circle 60 30)
			   (fd 30)
			   (circle -60 30)
			   (circle -90 30))
;27 tokens

;Calls make-strand inorder to draw a rainbow with thickness given
(define (make-rainbow thick)
	(make-strand 'red thick)
	(make-strand 'orange thick)
	(make-strand 'yellow thick)
	(make-strand 'green thick)
	(make-strand 'blue thick)
	(make-strand 'purple thick))
;21 tokens

;Makes a solid line of the rainbow, of the given color
(define (make-strand col thick)
	(begin_fill)
	(color col)
	(curve)
	(curve)
	(curve)
	(curve)
	(lt 90)
	(line thick 90)
	(rev-curve)
	(rev-curve)
	(rev-curve)
	(rev-curve)
	(lt 90)
	(line thick 180)
	(end_fill)
	(line thick 270))
;28 tokens

;Makes a full sized star at the given location (ex, why)
(define (star ex why) 
	(move (- ex 9) (- why 9))
	(goto (- ex 3) (- why 3))
	(move (+ ex 9) (- why 9))
	(goto (+ ex 3) (- why 3))
	(move (- ex 9) (+ why 9))
	(goto (- ex 3) (+ why 3))
	(move (+ ex 9) (+ why 9))
	(goto (+ ex 3) (+ why 3))
	)
;60 tokens

;Makes a small star at the given location (ex, why)
(define (small-star ex why)
	(move (- ex 5) (- why 5))
	(goto (+ ex 5) (+ why 5))
	(move (+ ex 5) (- why 5))
	(goto (- ex 5) (+ why 5))
	)
;32 tokens

;Makes a grid of stars starting from (ex, why) and ending at (-800, 800).
;The numbers in the code can be changed to alter width and end point.
;To save tokens, the code has a fixed endpoint.
(define (star-grid ex why start style)
	(cond ((and(< 800 ex)(> -800 why))(style ex why))
		  ((> -800 why)(style ex why)(star-grid (+ ex 100) start start style))
		  (else (style ex why)(star-grid ex (- why 100) start style))))
;41 tokens

;Makes a navy blue background
;Can be incorporated into draw to save 3 tokens, but decrease readability.
(define (make-background)
	(bgcolor 'navy)
	)
;21 tokens

;Draws cat
(define (nyan)
	(pencolor 'black)
	(pensize 5)
	(body 'navy)
	(tail)
	(shirt)
	(frontleg)
	(secondleg)
	(legspace)
	(backlegs)
	(headprep)
	(head)
	(glasses)
	(smile)
	)
;17 tokens

;Draws cat with denero
(define (nyandenero)
	(pencolor 'black)
	(pensize 5)
	(body 'yellow)
	(tail)
	(shirt)
	(frontleg)
	(secondleg)
	(legspace)
	(backlegs)
	(headprep)
 (fd 40)
    (setheading 0)
    (fd 45)
    (regshape 'denero_large.gif)

	)
;17 tokens

(define (body color)
	(fillcolor color)
	(begin_fill)
	(forward 70)
	(circle 10 90)
	(forward 140)
	(circle 10 90)
	(fd 70)
	(circle 10 90)
	(fd 140)
	(circle 10 90)
	(end_fill))

(define (shirt)
	(begin_fill)
	(fillcolor 'white)
	(lt 180)
	(fd 10)
	(setheading 0)
	(fd 30)
	(lt 90)
	(fd 160)
	(lt 90)
	(fd 20)
	(circle 10 90)
	(fd 150)
	(end_fill)
	(penup)
	(backward 20)
	(rt 90)
	(pendown)
	(pensize 3))

(define (frontleg)
	(setheading 220)
	(fillcolor 'white)
	(begin_fill)
	(pensize 4)
	(forward 40)
	(circle 5 180)
	(forward 45)
	(setheading 270)
	(forward 10)
	(end_fill))

(define (secondleg)
	(setheading 0)
	(lt 90)
	(penup)
	(forward 35)
	(pendown)
	(setheading 135)
	(fillcolor 'white)
	(begin_fill)
	(pensize 4)
	(forward 45)
	(circle 5 180)
	(forward 45)
	(lt 90)
	(forward 10)
	(end_fill))

(define (legspace) 
	(setheading 270)
	(penup)
	(forward 60)
	(pendown))

(define (backlegs)
	(setheading 170)
	(fillcolor 'grey)
	(begin_fill)
	(pensize 4)
	(forward 25)
	(circle 5 180)
	(forward 25)
	(lt 90)
	(forward 10)
	(end_fill)
	(setheading 0)
	(lt 90)
	(penup)
	(forward 30)
	(pendown)
	(setheading 200)
	(fillcolor 'grey)
	(begin_fill)
	(pensize 4)
	(forward 25)
	(circle 5 180)
	(forward 25)
	(lt 90)
	(forward 10)
	(end_fill)
	(setheading 0))

(define (headprep)
	(penup)
	(rt 90)
	(forward 102) 
	(pendown)
	(fillcolor 'grey))

(define (head)
	(begin_fill)
	(fd 50)
	(circle 20 90)
	(fd 50)
	(circle 6 145)
	(fd 30)
	(setheading 270)
	(fd 30)
	(rt 50)
	(fd 30)
	(circle 6 140)
	(fd 50)
	(circle 20 90)
	(end_fill))


(define (glasses)
	(penup)
	(lt 90)
	(fd 30)
	(rt 90)
	(pendown)
	(fillcolor 'black)
	(backward 18)
	(fd 48)
	(rt 90)
	(fd 6)
	(rt 180)
	(fd 6)
	(rt 90)
	(fd 36)
	(penup)
	(rt 90)
	(backward 6)
	(lt 270)
	(fd 15)
	(pendown)
	(begin_fill)
	(circle 7)
	(penup)
	(fd 40)
	(pendown)
	(circle 7)
	(end_fill)
	(penup)
	(lt 90)
	(fd 18)
	(pendown))

(define (smile)
	(fd 10)
	(lt 90)
	(fd 15)
	(lt 90)
	(fd 8)
	(backward 8)
	(rt 90)
	(fd 20)
	(lt 90)
	(fd 10))
	
(define (tail)
	(penup)
	(fd 30)
	(lt 90)
	(fd 160)
	(setheading 300)
	(pendown)
	(pencolor 'black)
	(fillcolor 'gray)
	(begin_fill)
	(forward 20)
	(circle 6 180)
	(fd 22)
	(end_fill)
	(penup)
	(setheading 90)
	(rt 90)
	(fd 28)
	(lt 90)
	(fd 174)
	(pendown))
	
(define (denero)
    (fd 40)
    (setheading 0)
    (fd 45)
    (regshape 'denero_large.gif))
	
; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.

(draw)
;1 token

;Using the counting script posted on piazza
;Total tokens => 679 