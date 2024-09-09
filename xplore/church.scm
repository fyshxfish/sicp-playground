(define zero 
    (lambda (f x) x)
)

(define (succ_result z f x) (f (z f x)) )

(define (succ z) (lambda (f x) (f (z f x))) )

(define one (lambda (f x) (f x)))

(define two (lambda (f x) (f (f x))))

(define thr (lambda (f x) (f (f (f x)))))

(define add_1 (lambda (x) (+ x 1)))