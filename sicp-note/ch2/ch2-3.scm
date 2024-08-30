; welcome to Section 2.3 
; wish you enjoy this meme:
;  user: say my name
;  bash: name 
;  user: I mean, say my $name 

#|
Section 2.3.1
|#

(define (memq item seq)
    (cond ((null? seq) false)
          ((eq? item (car seq)) seq)
          (else (memq item (cdr seq)))
    )
)

(define x '(this (is a) list))
(define y '(this (is a) list))

; E 2.54

(define (equal? x y) 
    (if (pair? x) 
        (and 
            (equal? (car x) (car y))    ; equal? cannot be replaced by eq?, because (car x) may be a pair
            (equal? (cdr x) (cdr y))
        )  
        (eq? x y)
    )
)

; (define f (car ''qq))
; (define x (f '(q w)))

; deriv

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same_variavle? exp var) 1 0))
          ((sum? exp) (make_sum (deriv (addend exp) var) 
                                (deriv (augend exp) var)))
          ((product? exp) (make_sum 
                                (make_product (multiplier exp) 
                                              (deriv (multiplicand exp) var))
                                (make_product (multiplicand exp) 
                                              (deriv (multiplier exp) var))))
          (else (error "unknown expression type: DERIVE" exp))
    )
)

(define variable? symbol?)

(define (same_variavle? v1 v2) 
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (make_sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          (else (list '+ a1 a2))
    )
)

; (define (make_sum a1 a2)
;    (list '+ a1 a2))



(define (make_product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2) ) (* m1 m2))
          (else (list '* m1 m2))
    )
)

(define (sum? x)
    (and (pair? x) (eq? '+ (car x)))
)

(define (product? x)
    (and (pair? x) (eq? '* (car x)))
)

(define (addend x) (cadr x))

(define (augend x) (caddr x))   ; 不是很想在这里写 point-free,

(define (multiplier x) (cadr x))

(define (multiplicand x) (caddr x))

(define (=number? x a)
    (and (number? x)
         (= x a)
    )
)

(define res (deriv '(* (* x y) (+ x 3)) 'x) )




