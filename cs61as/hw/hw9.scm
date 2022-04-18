#|
Exercise 1. Why did the student get an error?
The second is the correct code to create a new pointer for cdr.
The first is trying to get the cdr of a cons, not a mutable list type,
so it can't assign a value to a cdr of a list

|#

; Exercise 2
; Exercise 2a. Fill in the ?? so that the calls produce the desired effect.

(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))
(define (answer3)
  (set-cdr! list1 2)
  (set-cdr! list2 3))
(answer3)
list1 ; Should output ((a x b) b)
list2 ; Should output ((x b) y)
;Still trying to get this?? Sorry.

;Exercise 2b.  Draw a box-and-pointer diagram that explains the effect 
;              of evaluating
;(set-car! (cdr list1) (cadr list2)).
;(Reminder:  You can use ASCII art or submit a jpg or pdf file.)

;; 
;;  list1╭───┬───╮   ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ x │       │ y │
;;       ╰───╯       ╰───╯       ╰───╯       ╰───╯
;;

;Exercise 3. 
;SICP 3.13
;Draw the box-pointer diagram of z
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;;
;;          ╭────────────────────────────────╮
;;          │                                │
;;  z    ╭──V┬───╮   ╭───┬───╮   ╭───┬───╮   │
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼───╯
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮ 
;;       │ a │       │ b │       │ c │ 
;;       ╰───╯       ╰───╯       ╰───╯ 
;;

;What happens if we try to compute (last-pair z)?
;; Endless sequence: (a b c a b c a b ...


;SICP 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
  
;What does mystery do in general?
;It reverses the list

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;Draw the box-pointer diagram of v before the call to mystery, 
;v after the call to mystery, and w

;; 
;;  v    ╭───┬───╮   ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ c │       │ d │
;;       ╰───╯       ╰───╯       ╰───╯       ╰───╯
;;

;; Diagrams after defining w:
;; 
;;  v    ╭───┬───╮
;;  ────>│ ∘ │ ╱ │
;;       ╰─┼─┴───╯
;;         │      
;;       ╭─V─╮    
;;       │ a │    
;;       ╰───╯    
;; 
;;  w    ╭───┬───╮   ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ d │       │ c │       │ b │       │ a │
;;       ╰───╯       ╰───╯       ╰───╯       ╰───╯
;;

;What would be printed as the values of v and w?
;a
;'d c b a'


;Exercise 4.
;SICP 3.16 Draw the 4 box-and-pointer diagrams.
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
		 
 #|
a. Returns 3:
;; 
;;  p    ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ c │
;;       ╰───╯       ╰───╯       ╰───╯
;;

b. Returns 4:
;;
;;         ╭────────────────────────╮
;;         │                        │
;;  q    ╭─┼─┬───╮   ╭───┬───╮   ╭──V┬───╮
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ / │
;;       ╰───┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;                     │           │ 
;;                   ╭─V─╮       ╭─V─╮ 
;;                   │ b │       │ c │ 
;;                   ╰───╯       ╰───╯ 
;;
c. Returns 7:
;; 
;;  r    ╭───┬───╮
;;  ────>│ ∘ │ ∘ │
;;       ╰─┼─┴─┼─╯
;;         │   │
;;       ╭─V─┬─V─╮
;;       │ ∘ │ ∘ │
;;       ╰─┼─┴─┼─╯
;;         │   │
;;       ╭─V─┬─V─╮
;;       │ ∘ │ / │
;;       ╰─┼─┴───╯
;;         │
;;       ╭─V─╮ 
;;       │ c │ 
;;       ╰───╯ 
;;

d. Never returns:
;;
;;          ╭──────────╮
;;          │          │
;;  s    ╭──V┬───╮   ╭─┼─┬───╮   ╭───┬───╮
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ / │
;;       ╰─┼─┴───╯   ╰───┴───╯   ╰─┼─┴───╯ 
;;         │                       │ 
;;       ╭─V─╮                   ╭─V─╮ 
;;       │ a │                   │ c │ 
;;       ╰───╯                   ╰───╯ 
;;

|#

;SICP 3.17 Write a correct version of count-pairs.
(define (count-pairs x)
  (define pairs (make-hash-table))
  (define (count-pairs-1 x)
    (if (or (not (pair? x))
      (hash-table/get pairs x #f))
  false
  (begin 
    (hash-table/put! pairs x #t)
    (count-pairs-1 (car x))
    (count-pairs-1 (cdr x)))))
  (count-pairs-1 x)
  (hash-table/count pairs))


;SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;Ben's examples.  Then define print-queue.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
	  
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 
#| What happened with Ben's examples?

;; The Scheme printer prints q1 as ordinary pair. Its car is the list of the whole queue
;; and its cdr is the last item of the queue.

|#
; Implement the definition of print-queue
;Make sure you use display to print the queue.
(define (print-queue queue)
  (front-ptr queue))

;SICP 3.25 Write lookup and insert!

(define (lookup keys table)
    (if keys 
        (cdr table)
        #f))

(define (insert! keys value table)
    (if table 
        (set-cdr! table value)
        (set-cdr! table (cons (cons keys value) (cdr table)))))

#|
SICP 3.27

Explain why the number of steps is proportional to n (you may want to
include a trace to explain).
O(n) because it performs a constant number of operations per n. N is the number of nodes.
It goes down one branch until depth n, then it goes back up to the next branch, gets a value
and adds it to the next result up the tree. 

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?
No, fib will call itself instead of memo-fib. It inserts only one result into the table
But after the whole tree recursive process is done, it's already the final result.
|#

;Exercise 5. Write vector-append.
(define (vector-append v1 v2)
    (set-car! v1 )
    (set-cdr! v2 )
)

;Exercise 6. Write vector-filter.
(define (vector-filter pred vec)
    (list->vector (filter pred (vector->list vec))))

;Exercise 7. Write bubble-sort!
(define (bubble-sort! vec)
    (if (null? (cdr vec))   
        vec
        (if (< (car vec) (cadr vec))   
            (cons (car vec) (bubble-sort! (cdr vec)))   
            (cons (cadr vec) (bubble-sort! (cons (car vec) (cddr vec)))))))
; Starting from the first pair in the list, you move the larger value back until it is the last.
; The order of growth of the running time of bubble sort is Theta(n^2)
