(define (avr x y) (/ (+ x y) 2))

(define (cube x) (* x x x))

(define (search f neg-point pos-point)  ; correct already
    (define (close-enough? x)
        (< 
            (abs (f x)) 
            0.001
        )
    )

    (let (
            (mid (avr pos-point neg-point))
         )
        (cond 
              ((close-enough? mid) mid)         ; fix: remove the parenthesis around the second `mid`
              ((positive? (f mid)) (search f neg-point mid))
              (else (search f mid pos-point))
        )
    )
)

;;; xxx
(define (sqrt x)
    (fixed-point-transform 
        (lambda (y) (/ x (square y)))   ; > `sqrt 2`: why output is inifinite -nan.0? imagenary number?; `/` can be corrected to `-` 
        newton-transform
        1.4
    )
)

