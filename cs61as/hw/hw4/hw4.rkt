#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))
  ;(error "Not yet implemented"))

(define (lower-bound interval)
  (car interval))
  ;(error "Not yet implemented"))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;(error "Not yet implemented"))

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (z? y) 0
    (mul-interval x 
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))
  )

(define (z? x)
  (and (> 0 (lower-bound x)) (< 0 (upper-bound x))))

;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (let ((width (* c ( / tol 100.0))))
    (make-center-width c width)))

(define (percent i)
      (* (/ (interval-width i) (center i)) 100.0))
(define (interval-width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))
 
;  (error "Not yet implemented"))

; SICP 2.17 - Define last-pair

(define (last-pair lst)
   (let ((rest (cdr lst)))
    (if (null? rest)
        lst
        (last-pair rest))))
;(define nil (quote ()))

  ;(error "Not yet implemented"))

; SICP 2.20 - Define same-parity

(define (same-parity first . rest)
  (let ((first-is-even (even? first)))
    (define (same-parity-as-first? n)
      (let ((n-is-even (even? n)))
        (or (and first-is-even n-is-even)
            (and (not first-is-even) (not n-is-even)))))
    (define (build-list rest)
      (cond ((null? rest) nil)
            ((same-parity-as-first? (car rest))
             (cons (car rest) (build-list (cdr rest))))
            (else (build-list (cdr rest)))))
    (cons first (build-list rest))))

;(define (even? n)
 ; (= (remainder n 2) 0))

;(error "Not yet implemented. Do not forget to edit the arguments of this procedure as well."))

; SICP 2.22 - Write your explanation in the comment block:

#|
1) The squares are added to the front of the list (cons), so it builds from back to front.
2) The arguments for cons are flipped.
Your explanation here
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (cond ((null? lst) '())
        ((equal? old (car lst)) (list old (substitute cdr lst)))
        (else substitute (cdr lst))
))
  ;(error "Not yet implemented"))

; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  (cond ((equal? (length old) (length new)) '())
        ((null? lst) '())
        ((equal? old (car lst)) (list old (substitute cdr lst)))
        (else substitute (cdr lst))
))

  ;(error "Not yet implemented"))
