(define (tuple x y z)
    (define (match id)
        (cond ((= id 0) x) 
              ((= id 1) y)
              ((= id 2) z)
        )
    )
    match 
)

(define (mid t) (t 1))

; test:

(define t (tuple 5 7 1))
(define mid_t (mid t))