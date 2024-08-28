#|
Section 2.1.1 ; define retional 
|#
(define (add_rat x y)
    (make_rat (+ (* (numer x) (denom y)) 
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))          
    )
)

(define (sub_rat x y)
    (make_rat (- (* (numer x) (denom y)) 
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))        
    )
)

(define (mul_rat x y)
    (make_rat (* (numer x) (numer y)) 
              (* (denom x) (denom y))
    )
)

(define (div_rat x y)
    (make_rat (* (numer x) (denom y)) 
              (* (denom x) (numer y))
    )
)

(define (equal_rat x y) 
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))
    )
)

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(define (make_rat n d)
    (define (normalize_cons x y)    ;> E 2.1
        (let ((product (* x y)))
            (if (positive? product)
                (cons (abs x) (abs y))
                (cons (- (abs x)) (abs y))
            )        
        )
    )

    (let ((x (gcd n d)))
        (normalize_cons 
            (/ n x)
            (/ d x)
        )
    ) 
)

(define numer car)
(define denom cdr)
(define (print_rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
)

(define one_half (make_rat 1 2))
(define one_third (make_rat 1 3))
(define five_sixth (add_rat one_half one_third))

