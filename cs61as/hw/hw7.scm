(load "~cs61as/lib/obj.scm")

; 1 - Modify the person class.

(define-class (person name)
  (method (say stuff)
    stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name)))
  (method (repeat) (ask self 'say (se stuff))))


; 2 - Determine which definition works as intended.
; In particular, make sure the repeat method works.

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))))

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)))

(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))))

#|
Definition number ?? works as intended.
Your explanation here.
Number 3. It calls the variable twice and combines them into a sentence while 'saying it'
The first doesn't work due to repeat. The 2nd doesn't use say.
|#


; 3 - Write the random-generator class.
(define-class (random-generator random)
  (method (number) (random 10) (set! count (+ count 1)))
  (method (count) 'say count)
)

; 4 - Write the coke-machine class.
; For compatibility with the autograder, make sure that you display
; error messages.  That means you should say:
; (display "Not enough money") and
; (display "Machine empty") when appropriate.

(define-class (coke-machine coke-number price)
    (method (deposit amount) 
        (set! total (+ amount total))
        total)
    (method (fill input) 
        (set! coke-number (+ input coke-number))
        coke-number)
    (method (coke)
      (set! coke-number (- coke-number 1))
      (if (< deposit price)
        display "Not enough money")
      (if (< coke-number 1)
        display "Machine empty")
      (- amount price))
)

; 5 - Write the deck class.

(define ordered-deck
  (accumulate append '()
	      (map (lambda (suit)
		     (map (lambda (value) (word value suit))
			  '(A 2 3 4 5 6 7 8 9 10 J Q K)))
		   '(s d c h))))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (remove card deck))))))


(define-class (deck cards)
  (shuffle ordered-deck)
  (method (deal)
    (if (empty? deck)
      '()
    (return card 1 deck) ;removes the first card
    (remove card deck)))
  (method (empty?)
    (if (equals? (shuffle deck) '())
      #t
      #f)
  )
)

; 6 - Write the miss-manners class.
(define-class (miss-manners obj)
  (parent (manners obj))
    (method (please)
      obj))

