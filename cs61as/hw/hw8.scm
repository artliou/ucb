; SICP 3.3, 3.4 - Modify make-password-account to make it generate
; password-protected accounts.
; Also, if an incorrect password is given 7 times consecutively, you
; should say (call-the-cops).
; Note: In the case of a wrong password, you should return the string
; "Incorrect password".  Do not use display or print or error.

(define (make-password-account balance)
  (let ((number-of-incorrect-password 0))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (display-error amount)
    "Incorrect password")
  (define (call-the-cops amount)
    (display "call the cops"))
  (let ((password-list (list password)))
    (define (mkj new-password)
      (set! password-list (cons new-password password-list))
      dispatch)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'make-joint) mkj))
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
          (begin (set! number-of-incorrect-password
                       (+ 1  number-of-incorrect-password))
                 (if (> number-of-incorrect-password 7)
                     call-the-cops
        display-error))
  dispatch))
))

; SICP 3.7 - Define make-joint.
; You may want to modify make-password-account, but you shouldn't
; remove any of its previous functionality.

(define (make-joint account old-password new-password)
  ((account old-password 'make-joint) new-password))

; SICP 3.8 - Define reset-f!
; This is a function that defines the function f that the exercise
; asks for.

(define f #f)

(define (reset-f!)
  (set! f ??))

(define (initial-function)
  (let ((initial '()))
    (lambda (num)
      (if (null? initial)
  (begin
    (set! initial num)
    initial)
  initial))))

; For example, if you think that f should be the square function, you
; would say:
; (define (reset-f!)
;   (set! f (lambda (x) (* x x))))

; SICP 3.10 - Answer in the comment block.
#|
You have two options:
1) Draw the environment diagram here using ASCII art
2) Submit a .jpg or .pdf file containing your environment diagram (for
example, you could scan a paper drawing), in which case you should
write the filename here.

Environment diagram here
; Environment diagram
;
; global enviornment
; ----------------------------------------------------------------------------------------------------
; |   make-withdraw: -\                                                                              |
; |   /---------------/                                                                              |
; |   |                       (make-withdraw 100): -\                                                |
; |   |                       /---------------------/                                                |
; |   |                       |                                    W1: --\                           |
; ----|--->-------------------|------------------------------->----------|----------------------->----
;     |   |                   |       ---------------------   |          |       --------------  |
;     \->OO                   \->OO-->|initial-amount: 100|---/     / -> -->OO-->|balance: 100|--/
;        |                       |    ---------------------         |       |    --------------
;        >                       >                                          >
; params: initial-amount       params: balance                      |    params: amount
; body: ((l (balance)          body: (l (amount)  -- -- -- -- -- -- /    body: (if (> ...
;     (l (amount)                  ...)
;       ...))
;         initial-amount)                           
;
; This is different from earlier examples because the (let ((... creates an extra environment
; that is then discarded once (make-withdraw 100) is done. W1 is associated with the result of
; this.

Q. How do the environment structures differ for the two versions?
A. let creates an extra environment which is discard once make-withdraw is done.

Q. Show that the two versions of make-withdraw create objects with the
same behavior.
A. However, ultimately, the final product is that an object is created.

|#

; SICP 3.11 - Answer in the comment block.
#|
Same options as in 3.10

Environment diagram here

; Environment structure
;
; --------------------------------------------------
; | make-account: -\            acc: -\
; |   /------------/                  |
; |   |                               |
; |   |                               |
; ----|->-----------------------------|----------->-
;     | |                             |           |
;     | |                             |     --------------------
;     | |                             |     | balance: 50      |<----\
;     | |                             |     | withdraw: ----------->OO-------> params: amount
;     | |                             | /-->|                  |               body: (if ...
;     | |                             | |   |                  |<----\
;     | |                             | |   | deposit: ------------>OO-------> params: amount
;     | |                             | |   --------------------               body: (set!...
;     >OO                             >OO
;      |                               |
;      >                               >
; params: balance                      params: m
; body: ((define (withdraw...          body: (cond m...
;        (define (deposit...
;        (define (dispatch...
;        dispatch)

; ---------------------------------------------
; | make-account: ...
; | acc: ... --\
; -------------|-->---------------------------
;              |  |
; ------------->--|-                  --------------
; | balance: 50    |<-----------------| amount: 40 |
; | withdraw: ...  |                  |            |
; | deposit: ------------>OO--------->|            |
; ------------------      |           --------------
;                         |
;                         >
;                      params: amount
;                      body: (set!...
;
;
; ---------------------------------------------
; | make-account: ...
; | acc: ... --\
; -------------|-->---------------------------
;              |  |
; ------------->--|-                  --------------
; | balance: 90    |<-----------------| amount: 60 |
; | withdraw: ------------\           |            |
; |                |      >           |            |
; | deposit: ...   |      OO--------->|            |
; ------------------      |           --------------
;                         |
;                         >
;                      params: amount
;                      body: (if (...

Q. Where is the local state for acc kept?
A. Kept in its own environment

Q. How are the local states for the two accounts kept distinct?
A. When deposit or withdraw is called the resulting procedure also creates its own environment for the amount to be altered by.

Q. Which parts of the environment structure are shared?
A. Code deifnition. They don't share environments because of where each account/state is stored.

|#
