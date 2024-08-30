;> E 2.2

(define (print_point p) 
    (newline)
    (display "(" )
    (display (x_point p))
    (display ",")
    (display (y_point p))
    (display ")" )
)

(define make_point cons)

(define x_point car)
(define y_point cdr)


(define make_segment cons)
(define start_segment car)
(define end_segment cdr)


(define (avr x y)
    (/ (+ x y) 2)
)

(define (mid_point st ed)
    (make_point (avr (x_point st) (x_point ed))
                (avr (y_point st) (y_point ed))
    )
)

(define (midpoint_segment seg)
    (mid_point (start_segment seg) (end_segment seg))
)

; test: 
(define a_point (make_point 2 3))
(define b_point (make_point 4 9))
(define seg (make_segment a_point b_point))
(define midp (midpoint_segment seg))