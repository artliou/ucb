;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

;;Partner Solo: Arthur Liou. cs61as-fd

; Parent Class for Basic Objects
(define-class (basic-object) ; For Part B4a
  (instance-vars (properties (make-table)))
  
  (default-method 
     (lookup message properties))
    
  (method (put key value)
     (insert! key value properties))

  (method (get key)
     (lookup key properties)))

; Original Code/Methods
(define-class (place name)
  (parent (basic-object)) ; Part B4
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '())
  )
  (method (place?) #t) ;Problem B4B
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (map (lambda (person) (ask person 'notice new-person)) people)  ; A4 Part 1
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)

  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)

  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) 
  (method (may-enter? person) ; A4 Part 2
    #t)
)

(define-class (person name place)
  (parent (basic-object)) ; Part B4
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize
   (ask self 'put 'strength 100) ;Part B4 Part 1
   (ask self 'put 'money 100) ;A7
   (method (type) 'person)
   (ask place 'enter self))
  (method (person?) #t) ;Problem B4 Part 2
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))

  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
    ((not (memq thing (ask place 'things)))
     (error "Thing taken not at this place"
      (list (ask place 'name) thing)))
    ((memq thing possessions) (error "You already have it!"))
    (else
     (announce-take name thing)
     (set! possessions (cons thing possessions))
         
     ;; If somebody already has this object...
     (for-each
      (lambda (pers)
        (if (and (not (eq? pers self)) ; ignore myself
           (memq thing (ask pers 'possessions)))
      (begin
       (ask pers 'lose thing)
       (have-fit pers))))
      (ask place 'people))
         
     (ask thing 'change-possessor self)
     'taken)))

  (method (take-all) ; Part B3
    (if (null? (ask place 'things))
        (begin (display "nothing to take")
         (newline))
        (for-each (lambda (thg) (ask self 'take thg))
       (filter (lambda (thg) (eq? (ask thg 'possessor) 'no-one)) (ask place 'things)))))
  )
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
             (error "Can't go" direction))
            (else
               (if (equal? (ask new-place 'may-enter? self) #f) ; Part A4.2 Check Lock
                  #f
                 (begin (ask place 'exit self) (announce-move name place new-place)
                (for-each
                    (lambda (p) (ask place 'gone p) (ask new-place 'appear p)) 
                    possessions)
                  (set! place new-place)
                  (ask new-place 'enter self)
                  #t
                 )
                 )
               )
            )
      )
    )

; Question B6; Part 3
  (method (eat)
    (let ((items-to-eat (filter (lambda (thing) (if (ask thing 'edible?) #t #f)) (ask self 'possessions))))
      (let ((cal-items (map (lambda (thing) (ask thing 'calories)) items-to-eat)))
        (let ((total-calories (reduce + cal-items)))
    (ask self 'put 'strength (+ (ask self 'strength) total-calories))
    (map (lambda (thing) (ask self 'lose thing)) items-to-eat)))))

;A7 & 8 After Combining Work in Part 2
(method (get-money amount) ; A7 Part 1
    (ask self 'put 'money (+ (ask self 'money) amount)))
(method (pay-money amount) ; A7 Part 1
  (if (>= (ask self 'money) amount)
    (begin 
      (ask self 'put 'money (- (ask self 'money) amount))
      #t)
      #f))
  (method (buy food-name) ; A8
    (let ((selling (ask (ask self 'place) 'sell self food-name)))
      (if (equal? selling #f)
        (error "Can't buy this food item")
      (ask self 'take selling))))
  (method (go-directly-to new-place) ;; A-part2
     (ask place 'exit self)
     (announce-move name place new-place)
     (for-each
         (lambda (p)
           (ask place 'gone p)
           (ask new-place 'appear p)) possessions)
          (set! place new-place)
          (ask new-place 'enter self)
          #t)

;Question A4 & A5

(define-class (locked-place name) ;A4 Part 2
  (parent (place name))
  (instance-vars (locked #t))   
  (method (may-enter? person)
      (if (equal? locked #t)
        #f
      #t))
  (method (unlock)
    (set! locked #f)))
(define-class (garage name) ; A5 - Garage
  (parent (place name))
  (instance-vars (garage_table (make-table)))

  (method (park thing)  
    (if (memq thing (ask self 'things))
      (let ((tix (instantiate ticket)))
        (ask self 'appear tix)
        (ask (ask thing 'possessor) 'take tix)
        (insert! (ask tix 'serial_num) thing garage_table)
        (ask (ask thing 'possessor) 'lose thing))
    (error "The car not in garage"))
  )
  
  (method (unpark tix)
    (if (equal? 'ticket (ask tix 'name))
        (begin 
          (define serial (ask tix 'serial_num))
          (if (lookup serial garage_table)
            (begin
              (ask (ask tix 'possessor) 'take (lookup sn garage_table))
              (insert! sn #f garage_table)
              (ask (ask tix 'possessor) 'lose tix))
            (error "Can't unpark a car that was never parked there."))
          )
    (error "There is an error in accepting your command.")))
)

(define (ticket? item)
  (if (equal? 'ticket (ask item 'name))
    item
    #f))
        
(define-class (ticket)
  (parent (thing 'ticket))
  (class-vars (num_tickets 0))
  (instance-vars (serial_num num_tickets))
  (initialize (set! num_tickets (+ 1 num_tickets))))




; Part B4 (Part of B4)
(define-class (thing name)
  (parent (basic-object)) ; B4 Part 1
  (instance-vars (possessor 'no-one))
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor)
    (set! possessor new-possessor))
  (method (thing?) #t); B4 Part 2

) 

#|
(define thing
  (let ()
    (lambda (class-message)
      (cond
       ((eq? class-message 'instantiate)
	(lambda (name)
	  (let ((self '()) (possessor 'no-one))
	    (define (dispatch message)
	      (cond
	       ((eq? message 'initialize)
		(lambda (value-for-self)
		  (set! self value-for-self)))
	       ((eq? message 'send-usual-to-parent)
		(error "Can't use USUAL without a parent." 'thing))
	       ((eq? message 'name) (lambda () name))
	       ((eq? message 'possessor) (lambda () possessor))
	       ((eq? message 'type) (lambda () 'thing))
	       ((eq? message 'change-possessor)
		(lambda (new-possessor)
		  (set! possessor new-possessor)))
	       (else (no-method 'thing))))
	    dispatch)))
       (else (error "Bad message to class" class-message))))))
|#
; Part B5 - Hotspot Class

(define-class (hotspot name pass)
  (parent (place name))
  (instance-vars (alist '()) )
  (method (gone thing)
          (define (disconnect-distant laptops)
            (filter (lambda (laptop) (memq laptop (ask self 'things)))
                    laptops))
          (usual 'gone thing)
          (set! alist (disconnect-distant alist)))
  (method (connect laptop password)
          (cond ((and (equal? password pass)
                      (memq laptop (ask self 'things)))
                 (set! alist (cons laptop alist))
                 (display "Connection Successful") 
                 (newline))
                ((not (equal? password pass))
                 (display "Incorrect Password")
                 (newline))
                (else (display "Laptop not Here")
                      (newline))))
  (method (surf laptop url)
          (if (memq laptop alist)
              (system (string-append "lynx " url))
          )
  )
)

; Part 5B - Laptop Class
(define-class (laptop name) 
  (parent (thing name))

  (method (connect password)
          (cond ((equal? (ask self 'possessor) 'no-one)
                 (display "No owner")
                 (newline))
                (else
                 (ask (ask (ask self 'possessor) 'place) 'connect self password))))

  (method (surf url)
          (cond ((equal? (ask self 'possessor) 'no-one)
                 (display "No owner")
                 (newline))
                (else
                 (ask (ask (ask self 'possessor) 'place) 'surf self url))))
)

; Part - B6. 
; Part 1 DEfinition of food class
;Part 2 for making new foods
(define-class (food name cal)
  (parent (thing name)) ;Parent thing class
  (instance-vars
   (edible? #t))
  (initialize
   (ask self 'put 'calories cal))
  (method (food?) #t)
  (default-method
    (ask self 'get message)))

(define-class (bagel cal)
  (class-vars
   (name 'bagel))
  (parent (food 'bagel 50))
  (method (bagel?) #t)
  (default-method
    (ask self 'get message)))

(define-class (pasta cal)
  (class-vars
   (name 'pasta))
  (parent (food 'pasta 50))
  (method (pasta?) #t)
  (default-method
    (ask self 'get message)))

(define-class (fries cal)
  (class-vars
   (name 'fries))
  (parent (food 'fries 30))
  (method (fries?) #t)
  (default-method
    (ask self 'get message)))

(define-class (potstickers cal)
  (class-vars
   (name 'potstickers))
  (parent (food 'potstickers 75))
  (method (potstickers?) #t)
  (default-method
    (ask self 'get message)))

(define-class (coffee cal)
  (class-vars
   (name 'coffee))
  (parent (food 'coffee 25))
  (method (coffee?) #t)
  (default-method
    (ask self 'get message)))

;Question A7
(define-class (restaurant name food price)
  (parent (place name))
  (method (menu)
    (list (ask (instantiate food) 'name) price)) 
  (method (sell person food)
    (if (equal? (ask (instantiate food) 'name) food)
        (if (or (equal? (ask person 'type) 'police) (ask person 'pay-money price)) ; Question 9
            (let ((x (instantiate food)))
                  (ask self 'appear x)
                   x)
            #f)
         #f))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee fries pasta bagel))

(define (edible? thing)
  ;(ask food 'edible?))
  (member? (ask thing 'name) *foods*))

(define-class (thief initial-name initial-place)
  (parent (person initial-name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)
  (initialize
    (ask self 'put 'strength 200)) ; B7 Thief Default Strength
  (method (notice person) ;; A6 - No exits
    (if (eq? behavior 'run)
      (if (null? (ask (usual 'place) 'exits))
        (begin 
          (set! behavior 'steal)
          (print "You can't leave because you're in go-jail"))
        (ask self 'go (pick-random (ask (usual 'place) 'exits))))
	(let ((food-things
	       (filter (lambda (thing) c
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;Question B7 - Police Class
(define-class (police initial-name initial-place)
  (parent (person name initial-place))
  (initialize
   (ask self 'put 'strength 500))
  (method (type) 'police)
  (method (notice person)
    (if (equal? (ask person 'type) 'thief)
  (begin
    (display "No crime!")
    (map (lambda (thing)
     (ask self 'take thing))
         (ask person 'possessions))
    (ask person 'go-directly-to jail))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))

; Question 2E
(define-class (thing name)
  (instance-vars
   (possessor 'no-one))
  (parent (basic-object))
  (method (send-usual-to-parent)
    (error "Can't use usual without a parent" 'thing))
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor)
    (set! possessor new-possessor))
  (method (thing?) #t) ; Part B4
  (method (may-take? reciever) ;; B8
    (if (equal? possessor 'no-one)
      (print "No possessor")
      (let ((pos-str (ask possessor 'strength))
            (rec-str (ask reciever 'strength)))
      (if (> rec-str pos-str)
          self
          #f))))
  (default-method
    (ask self 'get message)))

; Question 2F
(define (whereis person)
  (ask (ask person 'place) 'name))

(define (owner some-thing)
  (if (eq? (ask some-thing 'possessor) 'no-one) 
    'no-one
      (ask (ask some-thing 'possessor) 'name))
)
(define (name obj) (ask obj 'name))
(define (inventory obj)
    (if (person? obj)
        (map name (ask obj 'possessions))
        (map name (ask obj 'things))))

