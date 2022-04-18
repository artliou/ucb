#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
    (cond ((empty? sent) '())
      ((member? (first sent) (bf sent)) (dupls-removed (bf sent)))
      (else (se (first sent) (dupls-removed (bf sent)))))
   ; Your code here
   ;(error "Not yet implemented")
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
	(cond ((empty? sent) 0)
		  ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
  		  (else (count-word (bf sent) wd))
  		  )
	)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here: 
It would fail because the new-if statement requires a different number of
arguments and the order of test, then-case, else-case isn't correct.
|#

; Exercise 4 - Define squares

(define (squares sent)
	(define (square x) (* x x))
  (if (empty? sent)
  	'()
  	(se (square (first sent))
  		(squares (bf sent))))
  ; Your code here
  ;(error "Not yet implemented")
)

; Exercise 5 - Define switch

(define (switch sent)
	(cond ((empty? sent) '())
      ((equal? (first sent) 'you) (se 'I (checks (bf sent))))
      ;((se (checks sent)) (switch (bf sent)))
		(else (se (first sent) (checks (sent))))
		)
  ; Your code here
 ;(error "Not yet implemented")
)

(define (checks sent)
	(cond ((equal? (first sent) 'I)
			(se 'you (switch (bf sent))))
     	((equal? (first sent) 'me)
     		(se 'you (switch (bf sent))))
	 	((equal? (first sent) 'you)
     		(se 'I (switch (bf sent))))
	 	(else checks (bf sent))
	)
)
; Exercise 6 - Define ordered?

(define (ordered? sent)
	(cond ((empty? sent) '())
		( (first sent) (first (bf sent)) #f)
		(ordered? (bf sent)))
  ; Your code here
  ;(error "Not yet implemented")
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
    (cond ((empty? sent) '())
      	((member? 'e (last (first sent))) (se (first sent) (ends-e (bf sent))))
      (else (ends-e (bf sent)))
      )
       ; Your code here
  ;(error "Not yet implemented")
)

; Exercise 8

#|
Your explanation here
That way the run time could be cut down and only the first argument that returns true/false
would be necessary. For an ordinary function, it would help determine the number of
true or false values (counter)
|#
