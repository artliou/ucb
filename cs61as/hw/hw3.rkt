#lang racket

(require berkeley)
(provide (all-defined-out))

;NOTE. I'm sorry that not many of the exercises will pass the usertests
;I'm a late add and I'm trying to finish all (with some effort) before tonight's deadline.

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n a)
  (cond ((< n 1) a)
    ((even? n) (square (fast-expt-iter b (/ n 2) a)))
    (else (* a (fast-expt-iter b (- n 1) a)))
    )
  ; Your code here
  ;(error "Not yet implemented")
)


; Exericse 2 - Define phi

(define (phi)
  (error ("Optional"))
  ; Your code here
  (error "Not yet implemented")
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
    (cond ((< n k) 0)
    (( (cont-frac n d (- k 1))))
    (else (cont-frac n d (- k 1) )))
  ; Your code here
  ;(error "Not yet implemented")
)

;; Iterative version
(define (cont-frac-iter n d k)
  (cond ((< n k) 0)
  (( (cont-frac n d (- k 1))))
  (else (cont-frac n d (- k 1) )))  
  ; Your code here
  ;(error "Not yet implemented")
)

(define (e k)
  (+ 2 (cont-frac 1 e k ))
  ; Your code here to estimate e using cont-frac with k terms.
  ;(error "Not yet implemented")
)

; Exercise 4 - Define next-perf

(define (next-perf n)
  (if (= (sum-of-factors 0 0 (+ n 1) (+ n 1)))
    n
    (next-perf (+ n 1)))
  ;  Your code here
  ;(error "Not yet implemented")
)

(define (sum-of-factors sum counter n)
    (cond ((> counter n) sum)
      (= (remainder n counter) 0) (sum-of-factors (+ sum counter) (+ counter 1) n))
      (sum-of-factors sum (+ counter 1) n)
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here
It would start counting off in a different way. it'd check to see if there is any amount of if the kinds of coins are empty.
Then it would check the amount (if it was 0).
Just those two are switched from the original, and in this case, it could reset the return/response
By giving 0, and then 1 (because of the order it is executed in)
|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt: The result is b^counter*counter! (B(exponentially countered) times counter factorial)

Formula for expt-iter: The result is product times 2^Counter. 

|#
