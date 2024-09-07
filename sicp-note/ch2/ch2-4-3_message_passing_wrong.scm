; wrong 
; Boolean in the view of message passing 

(define tru

    (define (dispatch op) 
        (cond ((eq? op 'logic_and) logic_and)
              ((eq? op 'logic_or)  logic_or)
        )
    )

    (define (logic_and x)
        (if (eq? x tru)
            tru
            fls
        )
    )

    (define (logic_or x)
        tru
    )

    dispatch

)

(define fls

    (define (dispatch op) 
        (cond ((eq? op 'logic_and) logic_and)
              ((eq? op 'logic_or)  logic_or)
        )
    )

    (define (logic_and x)
        fls
    )

    (define (logic_or x)
        (if (eq? x tru)
            tru
            fls
        )
    ) 

    dispatch

)

; ((fls 'logic_or) tru) 


; $ in HS? ****** 

(define t tru)
(define f fls)
; ((t 'logic_or) f) 