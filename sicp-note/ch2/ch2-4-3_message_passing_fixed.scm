; Boolean in the view of message passing 

(define (tru_logic_and x)
    (if (eq? x tru)
        tru
        fls
    )
)

(define (tru_logic_or x)
    tru
)

(define (fls_logic_and x)
    fls
)

(define (fls_logic_or x)
    (if (eq? x tru)
        tru
        fls
    )
) 

(define tru
    (lambda (op) 
        (cond ((eq? op 'and) tru_logic_and)
              ((eq? op 'or)  tru_logic_or)
        )
    )
)


(define fls
    (lambda (op) 
        (cond ((eq? op 'and) fls_logic_and)
              ((eq? op 'or)  fls_logic_or)
        )
    )
)


; ((fls 'or) tru) 
; ((tru 'or) fls) 
; ((((fls 'or) tru) 'and) fls)

(define (logic op x y) ((x op) y))
; (logic 'and tru fls)
; (logic 'and tru tru)
; (logic 'and fls tru)
; (logic 'and fls fls)
; (logic 'or (logic 'and fls fls) tru)

; $ in HS? ****** 
