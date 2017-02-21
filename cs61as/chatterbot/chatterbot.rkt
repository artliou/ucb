#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    (se sent)
    ;;insert your answer here
    ;(error "not yet implemented")
  )

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    (lambda (x) (se motto))
    ;;insert your answer here
    ;(error "not yet implemented")
  )

;;Q3 - matcherbot-creator
  (define (matcherbot-creator pattern)
    (lambda (sent)
        (cond ((empty? pattern) sent) ;If PATTERN is the empty sentence, return SENT.
          ;((equal? pattern (first sent)) (se (sent)))) ;Return everythings after sent          
          ((equal? pattern (last sent)) '()) ;If PATTERN is at the very end of the sentence, return the empty sentence. 
          ((membersent pattern sent) (checkfirst pattern sent)) ;If PATTERN is not in SENT, return #f. 
          (else #f))
    ;;insert your answer here
    ;(error "not yet implemented")
    ))

(define (membersent pattern sent)
  (cond ((empty? pattern) #t)
        ((empty? sent) #f)
        ((equal? (first pattern) (first sent))
          (membersent (bf pattern) (bf sent)))
          (else (membersent pattern (bf sent))))
  )

(define (checkfirst pattern sent)
    (cond ((empty? sent) '())
          ((empty? pattern) sent) 
          ((equal? (first pattern) (first sent))
            (se '() (checkfirst (bf pattern) (bf sent))))
          (else (checkfirst pattern (bf sent)))))

;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    (lambda (x) (substitutebot from to x))) ; Calls a New function

; New Function 
  (define (substitutebot from to sent) ;Define this function
      (cond ((and (empty? from)(empty? to)) sent)
        ((member? (first sent) from)
          (se (switch (first sent)) (substitutebot (bf sent) from to)))
        (else (se (first sent) (substitutebot (bf sent) from to)))
      )
  )
  ;(define (word? x)
      ;(if (empty? x)
       ; '()
      ;(member? x from)
     ; )
    ;)
  (define (switch senter) ; Switch function
      (if (empty? senter)
          '()
          ((first senter) (switch (bf senter))) 
        )
  )

;Check if in sent? if so, then switch
  ;;insert your answer here
  ;(error "not yet implemented")

;;Q5 - switcherbot
  (define (switcherbot sent)
    (cond ((empty? sent) '())
      ((equal? (first sent) 'you) 
        (se 'I (switcherbot2 (bf sent))))
    (else (switcherbot2 sent)
    ;(else (se (switcherbot2 sent) (switcherbot2 (first sent)))
    )
    ;;insert your answer here
    ;(error "not yet implemented")
    )
  )

  (define (switcherbot2 sent)
    (cond ((empty? sent) '())
      ((equal? (first sent) 'me)
        (se 'you (switcherbot2 (bf sent))))

    ((equal? (first sent) 'I)
        (se 'you (switcherbot2 (bf sent))))

    ((equal? (first sent) 'are)
        (se 'am (switcherbot2 (bf sent))))
      ((equal? (first sent) 'am)
        (se 'are (switcherbot2 (bf sent))))

    ((equal? (first sent) 'was)
        (se 'were (switcherbot2 (bf sent))))
    ((equal? (first sent) 'my)
        (se 'your (switcherbot2 (bf sent))))
    ((equal? (first sent) 'yours)
        (se 'mine (switcherbot2 (bf sent))))

    ((equal? (first sent) 'you)
        (se 'me (switcherbot2 (bf sent))))
    (else (se (first sent) (switcherbot2 (bf sent))))
    ;;insert your answer here
    ;(error "not yet implemented")
    )
  )

;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    (if (empty? sent)
      '()
        (se (switcherbot sent) '?)
    )
    ;;insert your answer here
    ;(error "not yet implemented")
  )
  
;;Q7 - eliza
  (define (eliza sent)
    (cond ((empty? sent) (se '(how can I help you ?)))
      ((equal? 'hello (first sent)) (se '(hello there!)))
      ((equal? '? (last sent)) (se '(I can not answer your question.)))
      ((equal? 'I (first sent)) (se '(why are you) (switcherbot (bf (bf sent))) '?))
      (else (se 'why (switcherbot sent) '?))
    )
    ;;insert your answer here
    ;(error "not yet implemented")
  )

(define (sents)
  (cond ((empty? (first sents)) #f)
        ((empty? (bf sents)) #f)
        (else (equal? '(I am) (se (first sents) (first (bf sents)))))
        ))

(define (phrase? f s)
  (cond ((or (empty? f)(empty? s)) '())
        (equal? (first f) (f second))
            (phrase? (first (bf f)) (first (bf s)) )
        (else #f)

  )
)
;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    (lambda (sent)
        (cond ((empty? bot) '())
          ((equal? sent pat) out)
          (else (bot sent)
        )))
    ;;insert your answer here
    ;(error "not yet implemented")
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
      (lambda (sent)
        (cond ((empty? bot) '())
          ((equal? (first pat) (first sent))
            (se before (bf (bf sent)) after))
          (else (bot sent))
        )
      )
        ;;insert your answer here
    ;(error "not yet implemented")
  )

;;Q10 - exagerate
  (define (exaggerate bot n)
    (lambda (sent)
        (cond ((= n 0) '() sent)
          ((empty? sent) '())
          ((adjective? (first sent)) (se 'very (first sent) ((exaggerate bot n)(bf sent))))
          ((se (first sent) ((exaggerate bot n)(bf sent))))
          (else (exaggerate bot (- n 1)))

        )
    )

    ;;insert your answer here
    ;(error "not yet implemented")
  )
;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
