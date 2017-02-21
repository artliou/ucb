;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate place 'Noahs))
(define Intermezzo (instantiate place 'Intermezzo))
(define Haas (instantiate place 'Haas-Business-School))
(define s-h (instantiate place 'sproul-hall))

;Question 1
(define Dorm (instantiate place 'Dorm))
(define Kirin (instantiate place 'Kirin))

;Question  A6
(define jail (instantiate place 'jail))

;Directions for Kirin
(can-go Soda 'north kirin)
(can-go kirin 'south Soda)
(can-go Soda 'east Dorm) ;So live next to Soda on the east side
(can-go Dorm 'west Soda)

(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-office 'west art-gallery)
(can-go Soda 'down 61A-Lab)
(can-go 61A-Lab 'up Soda)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south Haas)
(can-go Haas 'north Pimentel)
(can-go Haas 'west s-h)
(can-go s-h 'east Haas)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-lab))
(define nasty (instantiate thief 'nasty sproul-plaza))

;Question 1
(define Arthur (instantiate person 'Arthur Dorm))

(define (sproul-hall-exit)
   (let ((check 3))
	 (lambda ()
	   (if (> check 0)
	       (begin (set! check (- check 1))
		      (error "You can check out any time you'd like, but you can never leave"))
	       '()
	    )))
)

(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))
(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))
(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))

;; Some things.

(define bagel (instantiate thing 'bagel))
(ask Noahs 'appear bagel)

(define coffee (instantiate thing 'coffee))
(ask Intermezzo 'appear coffee)


;New Items
(define potstickers (instantiate thing 'potstickers))
(ask kirin 'appear potstickers)

(define MacBookPro (instantiate laptop 'MacBookPro))
(ask Dorm 'appear MacBookPro)
(define iPhone (instantiate thing 'iPhone))
(ask Dorm 'appear iPhone)
(define diet-coke (instantiate thing 'diet-coke))
(ask Sproul-Plaza 'appear diet-coke)

;Part of A4P1
;(define singer (instantiate person 'rick sproul-plaza))
;(ask singer 'set-talk "My funny valentine, sweet comic valentine")
;(define preacher (instantiate person 'preacher sproul-plaza))
;(ask preacher 'set-talk "Praise the Lord")
;(define street-person (instantiate person 'harry telegraph-ave))
;(ask street-person 'set-talk "Brother, can you spare a buck")
