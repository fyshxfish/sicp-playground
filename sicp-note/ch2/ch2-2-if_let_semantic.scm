; 其实是let的语义, let会直接严格求值, 即使你把let嵌套在内层函数而且这个函数还完全没有被调用.

; wrong code archive:

#|
(define (prime? x) ; (prime? 1) 积极求值
    (if (= x 1)
        #t
        test_prime   
    )

    (define (divisible? y)
        (= 0 (remainder x y))
    )
   
    (define (iter_biggest_divisor y)
        (cond ((= y 1) 1)
              ((divisible? y) y)
              (else (iter_biggest_divisor (- y 1)))
        )
    )
    
    (define test_prime
        (let ((biggest_divisor (iter_biggest_divisor (quotient x 2)) ))
             (display biggest_divisor)
             (if (= biggest_divisor 1)
                 #t
                 #f
             )
        )
    )
)
|#


; test 
(define (comp x)  
    (if (> 3 x)
        (display "then-clause")
        (display "else-clause")
    )

#|
    (define foo1
        (let ((bar1 (/ 2 0)))    ; * evaluted conditionlessly
            (display "should not be printed")
        )
    )
|#
    (define foo2
        (let ((bar2 (/ 5 2)))    ; * evaluted conditionlessly
            (newline)
            (display "let in `foo2`, bar2: ")
            (display bar2)
        )
    )
)