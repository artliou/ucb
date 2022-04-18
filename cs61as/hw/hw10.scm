;;Lesson 10

;;Exercise 1
;;SICP 3.5.1
#| Explain
Both are streams.
1 - Value is delayed. More like a lambda
2 - Value is forced immediately. Procedure

|#

;;Exercise 2
;;SICP 3.5.1
#| Explain
The constructor, cons-stream, is within the argument for stream-cdr. 
The time at which the elements are evaluated is not correct. 
the cdr is evaluated at selection time, not construction time.
|#

;;Exercise 3
#| Explain
It is delayed
stream creates a pair, with the promise to evalute/return later.
Stream is the stream analog of enumerate-interval
|#

;;Exercise 4
;a.
(define (num-seq n)
  (define (stone n)
    (cond ((odd? n) (+ 1 (* 3 n)))
          ((even? n) (/ n 2))))
  (if (= n 1)
      (list n)
      (append (list n) (num-seq (stone n)))))

(define even?
  (lambda (number)
    (= (remainder number 2) 0)))

(define odd?
  (lambda (number)
    (not (even? number))))

;b.
(define (seq-length stream)
  (let (count 0)
  (if (= (stream-car (hailstone stream)) 1)
    count
    (set! count (+ count 1))
    (seq-length (stream-cdr stream)))))

;;Exercise 5
;;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;3.51
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
#| Returns:
1
2
3
4
5

|#
(stream-ref x 7)
#| Returns:
6
7

|#


;;3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

#| What is the value for 'sum'?
1

|#

#| What is the printed response to evaluating the stream-ref and display-stream?
stream-ref
136
display-stream
10, 15, 45, 55, 120, 190 and 210

|#

#| Will it be diffferent if we implemented (delay <exp>) as (lambda () <exp>)
Yes they differ, because seq is then evaluated twice by y and z.

|#
;;3.53
#|Describe the elements of the stream
(define s (cons-stream 1 (add-streams s s)))

 Let's denote the n-th element of s as s(n).
s(1) = 1, and
s(n) = s(n - 1) + s(n - 1) by the definition.

So,
s(2) = s(1) + s(1) = 2
s(3) = s(2) + s(2) = 4
s(4) = s(3) + s(3) = 8
s(5) = s(4) + s(4) = 16
and so on
s(n) = s(n - 1) + s(n - 1) = 2^(n - 1)
It goes on to keep doubling.
|#

;;3.54
(define (mul-streams s1 s2)
(if (or (stream-null? s1) (stream-null? s2))
    the-empty-stream
    (cons-stream (* (stream-car s1) (stream-car s2))
                 (mul-streams (stream-cdr s1) (stream-cdr s2)))))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;;3.55
(define (partiam-sums stream)
  (if (stream-null? stream)
    0
    (cons-stream (stream-car stream)
                 (stream-map (lambda (x) (+ x (stream-car stream)))
                             (partial-sums (stream-cdr stream))))))
;;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream integers 2)
                                       (scale-stream integers 3))
                                (scale-stream integers 5))))

;;3.64
(define (stream-limit stream tolerance)
  (let ((s2 (stream-cdr s)))
    (if (< (abs (- (stream-car s)
                   (stream-car s2)))
           tolerance)
        (stream-car s2)
        (stream-limit s2 tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;3.66
#| Explain
pairs starts with ((car s) (car t)) and then interleaves
There is a mathematical formula for which pair precedes another.

|#

;;3.68
#| Explain
Won't work. evaluating the function will create a infinite loop.
interleave still needs the first value from each stream, so it'll keep calling
pairs recursively
|#


;;Exercise 6
(define (fract-stream lst)
  (if (> (stream-car lst) (stream-cdr lst))
      (error "Numerator is greater than denominator")
    (let stream (/ (stream-car lst) (stream-cdr lst))
        (cons-stream (stream-car stream)
                 (stream-map (lambda (x) (+ x (stream-car stream)))
                             (fract-stream (stream-cdr stream)))))))

(define (approximation stream lst)
    (if (stream-null? stream)
    0
    (cons-stream (stream-car (fract-stream stream))
                 (stream-map (lambda (x) (+ x (stream-car stream)))
                             (partial-sums (stream-cdr stream))))))


;;Optional Challenge Problem.
;;Note: this is NOT extra credit. Only do it if you want more practice.
#|
  Your explanation Here
  I don't see the EC problem on the webpage.
|#
