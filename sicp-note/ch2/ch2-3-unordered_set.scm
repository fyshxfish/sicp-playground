#| Section 2.3.3 Example: Representing Sets |#

(define (elem x s)
    (cond ((null? s) false) 
          ((equal? x (car s)) true)
          (else (elem x (cdr s)))
    )
)

(define (adjoin x s)
    (if (elem x s)
        s 
        (cons x s)
    )
)

(define (union s1 s2)
    (cond ((null? s1) s2)
          ((elem (car s1) s2) (union (cdr s1) s2))
          (else (cons (car s1) (union (cdr s1) s2)))
    )
)

(define (intersection s1 s2)
    (if (or (null? s1) (null? s2)) 
        '() 
        (if (elem (car s1) s2)
            (cons (car s1) (intersection (cdr s1) s2))
            (intersection (cdr s1) s2)
        )
    )
)

(define s1 (list 1 2 3 4))
(define s2 (list 4 5 7 2))
