(define zero 
    (lambda (f x) x)
)

(define one (lambda (f x) (f x)))

(define (succ_result z f x) (f (z f x)) )

(define (succ z) (lambda (f x) (f (z f x))) )


(define two (lambda (f x) (f (f x))))

(define thr (lambda (f x) (f (f (f x)))))

; (define (succ z) (lambda (f x) (f (z x))))

; (define (succ z f x) (f (z x)) )


(define square (lambda (x) (* x x)))


(define add_1 (lambda (x) (+ x 1)))