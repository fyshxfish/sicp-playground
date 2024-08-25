#| 
Section 1.2
|#


(define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))
    )
)

(define (factorial2 n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* n product) (+ counter 1))
        )
    )
    (iter 1 1)
)


(define (exp c n)
    (define (exp-iter product count)
        (if (= count 0) 
            product
            (exp-iter (* c product) (- count 1))
        )
    )
    (exp-iter 1 n)
)