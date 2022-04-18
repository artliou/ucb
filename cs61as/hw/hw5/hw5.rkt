#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.
;(1 2 3 4 5 6)
;((1 2 3) 4 5 6)
;((1 2 3) (4 5 6))

; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (car mobile))
  ;(error "Not yet implemented"))

(define (right-branch mobile)
  (car (cdr mobile)))
  ;(error "Not yet implemented"))

(define (branch-structure branch)
  (car branch))
  ;(error "Not yet implemented"))

; b. Define total-weight.

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile)))) 
;(error "Not yet implemented"))

(define (mobile? structure) (pair? structure))
 
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

; c. Define balanced?

(define (balanced? mobile)
    (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (if (mobile? (branch-structure left)) (balanced? left) #t)
         (if (mobile? (branch-structure right)) (balanced? right) #t))))
  ;(error "Not yet implemented"))

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (branch-length branch) (car branch))

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;NEW CODE
;We have to change the selectors
;(define (left-branch mobile) 
 ; (car mobile))
 
;(define (right-branch mobile) 
 ; (cdr mobile))
 
;(define (branch-length branch) 
 ; (car branch))
 
;(define (branch-structure branch) 
 ; (cdr branch))

;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (cond ((null? d-l) nil)
        ((pair? d-l) (cons (square-tree (car d-l))
                            (square-tree (cdr d-l))))
        (else (square d-l))))
;  (error "Not yet implemented"))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
    (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))
  ;(error "Not yet implemented"))

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
 
;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (dot-product m v) m))

(define (transpose mat)
  (accumulate-n (dot-product (car mat) (cdr mat)) (matrix-*-vector (car mat) mat)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (dot-product m n) m)))

;Exercise 6 - Give the property that op should satisfy:

#|

Your property here
It should be cummutative.

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        ((or (pair? l1) (pair? l2))
         #f)
        ((and (null? l1) (null? l2))
         #t)
        ((or (null? l1) (null? l2))
         #f)
        (else (eq? l1 l2))))

  ;(error "Not yet implemented"))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (subsets (cdr s)) rest)))))

;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
  (else (calc-apply (calc-eval exp)))))

  ;Changing this Part!
  ;(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
