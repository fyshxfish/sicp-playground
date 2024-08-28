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