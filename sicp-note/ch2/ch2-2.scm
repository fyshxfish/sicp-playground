#|
Section 2.2
|#

; list 

(define l (list 5 601 3 1))
(define l2 (list 9 7 5 314))

(define (list_ref l n)
    (if ( = n 0)
        (car l)
        (list_ref (cdr l) (- n 1))
    )
)

(define (len l)
    (define (iter lst count)
        (if (null? lst)
            count 
            (iter (cdr lst) (+ count 1))
        )
    )
    (iter l 0)
)

(define (len_r l)
    (if (null? l)
        0
        (+ 1 (len_r (cdr l)))
    )
)

(define (apend lhs rhs)
    (if (null? lhs)
        rhs
        (cons (car lhs)
              (apend (cdr lhs) rhs) )
    )
)


;> E2.17

(define (last_pair l)
    (if (null? (cdr l))
        (car l)
        (last_pair (cdr l))
    )
)

;> 2.18
; HS! Pattern Match, Miss you so...
(define (revrse l)
    (define (iter lst accu)
         (if (null? lst)
             accu
             (let ((head (car lst))
                   (tail (cdr lst)))
              (iter tail (cons head accu))
             )     
         )
    )
    (iter l (list))
)

;> E 2.20
(define mod remainder)

(define (eq_bool b1 b2)
    (if (or (and b1 b2) (and (not b1) (not b2)))
        true
        false
    )
)

(define (same_parity head . tail)

    (
        let ((test (even? head)))
        (define (iter lst accu)
            (if (null? lst)
                accu
                (if (eq_bool test (even? (car lst)))
                    (iter (cdr lst) (cons (car lst) accu))
                    (iter (cdr lst) accu)
                )            
            )
        )
        (revrse (iter tail (list head)))
    )
)


