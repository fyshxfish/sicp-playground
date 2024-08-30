(define (elem x s)
    (cond ((= x (car s)) true)
          ((> x (car s)) false)
          (else (elem x (cdr s)))
    )
)

(define (intersection s1 s2)
    (cond ((or (null? s1) (null? s2)) ()) 
          ((= (car s1) (car s2)) 
                (cons 
                    (car s1) 
                    (intersection (cdr s1) (cdr s2))
                )
          )
          ((< (car s1) (car s2)) 
                (intersection (cdr s1) s2) 
          )
          (else (intersection s1 (cdr s2)))
    )

)

(define (adjoin x s)        ; (adjoin 7 s2)
    (cond ((null? s) (list x))
          ((= x (car s)) s)
          ((< x (car s)) (cons x s))
          (else (cons (car s) (adjoin x (cdr s))))
    
    )

)

(define s1 (list 1 5 7 9))
(define s2 (list 2 3 5 9))