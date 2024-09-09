(define square3 (lambda (x) (square (square (square x)))) )

(define three_times_f (lambda (f x) (f (f (f x)))))

(define zero_time_f (lambda (f x) x))
(define one_time_f (lambda (f x) (f x)))
(define two_times_f (lambda (f x) (f (f x))))

(define (succ z) (lambda (f x) (f (z f x))))

(define (church n) 
    (if (= n 0)
        zero_time_f
        (succ (church (- n 1)))
    )
)
