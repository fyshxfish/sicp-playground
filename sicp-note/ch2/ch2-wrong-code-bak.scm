(define (print_point p) ( 
    (newline)
    (display "(" )
    (display (x_point p))
    (display ",")
    (display (y_point p))
    (display ")" )
                        ) ; this pair of parenthesis should be removed 
)

(define make_point cons)

(define x_point car)
(define y_point cdr)

(define a_point (make_point 2 3))

#|
7 error> (print_point a_point) 
)3,2(       ;;; ##### why this? #####
;The object #!unspecific is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 8) => Specify a procedure to use in its place.
; (RESTART 7) => Return to read-eval-print level 7.
; (RESTART 6) => Return to read-eval-print level 6.
; (RESTART 5) => Return to read-eval-print level 5.
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
|#



(define (square_tree t) (tree_map square t))    ; NO CURRYING?


(define (sum_tree_nodes t)
    (cond ((null? t) 0)     ; 为什么这里要考虑null?
          ((pair? t) (+ (sum_tree_nodes (car t))
                        (sum_tree_nodes (cdr t))) )
          (else t)  
    )
)

(define (square_tree t)         ; 而这里不考虑null? 或者这里的不考虑本来就是一种疏忽? 
    (map (lambda (sub_t) 
            (if (pair? sub_t)
                (square_tree sub_t)
                (* sub_t sub_t)  
            )
         ) 
    t)
)


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

; imperative code in fp

(define (union s1 s2)
    (map (lambda (x) (adjoin x s2)) s1)
)