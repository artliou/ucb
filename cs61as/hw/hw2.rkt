#lang racket

(require berkeley)
(provide (all-defined-out))


;PLEASE READ
;NOTE. I'm sorry that not many of the exercises will pass the usertests
;I'm a late add, trying to get through all the stress of crunch deadlines
; and I'm trying to finish all my assignments (with significant) before tonight's deadline.


; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  (cond ((empty? sent) '())
      ((equal? (first sent) old-word) (se new-word (substitute (bf sent) old-word new-word)))
      (else (se (first sent) (substitute (bf sent) old-word new-word)))
  )
  ; Your code here
  ;(error "Not yet implemented")
)


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: #<procedure>

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns: 25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 2

Type of value returned by g: <function>

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
(define f1 10)
(define f2 (lambda x x))
(define f3 (lambda (x) (+ x 3)))
(define f4 ((lambda x x)))
(define f5 (lambda x (lambda x (+ x 3))))

; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester
;Write and test the make-tester procedure. 

(define (make-tester wd) 
  (lambda (x) (equal? wd x))
  ;Your code here
  ;(error "Not yet implemented")
)

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
  (if (< a b)
    0
    (* a (product (* term 1) b)))
  ; Your code here
  ;(error "Not yet implemented")
)

(define (estimate-pi)
  ; NOT SURE HOW TO DO
  (3.1415925359)

  ; Your code here
  ;(error "Not yet implemented")
)

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  (if (< term next) null-value
          
      (combiner (- term 1) a next b))
  ; Your code here
  ;(error "Not yet implemented")
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (if (< term next)
    term
  (sum-accum (- term 1) a next b))
  ; Your code here
  ;(error "Note yet implemented")
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (if (< term next)
    product
  (sum-accum (- term 1) a next b))
  ; Your code here
  ;(error "Note yet implemented")
)


; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  ; Your code here
  (error "Not yet implemented")
)

(define (sum-sq-prime a b)
  (+ (square a) (square b))
  ; Your code here
  ;error "Not yet implemented")
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (* n n)
  ; Your code here
  ;(error "Not yet implemented")
)

; SICP 1.40 - Define cubic

(define (cubic a b c)
  (quotient (sqrt (- (square b) (* 4 a c))) (* a 2))
  ; Your code here
  ;(error "Not yet implemented")
)

; SICP 1.41 - Define double

(define (double proc)
  (proc proc)
  ; Your code here
  ;(error "Not yet implemented")
)

; SICP 1.43 - Define repeated

(define (my-repeated proc n)
  (if (> n 0) 
    proc n)
    (proc (- n 1))
  ; Your code here
  ;(error "Not yet implemented")
)

; Exercise 9 - Define my-every

(define (my-every proc sent)
  (proc sent)

  ;not sure??? This would call the function of proc with sent as the argument right?
  
  ; Your code here
  ;(error "Not yet implemented")
)

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: '(pp uu rr pp ll ee)

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns: invalid arguments to member?

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#
