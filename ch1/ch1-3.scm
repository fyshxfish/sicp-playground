#|
Section 1.3
|#

(define (sum-ints a b)
    (if (> a b)
        0
        (+ a (sum-ints (+ a 1) b))
    )
)

;;; template / abstraction

(define (sum a b f update)
    (if (> a b)
        0
        (+ (f a) (sum (update a) b f update))
    )
)

(define (cube x) (* x x x))
 
(define (inc x) (+ x 1))

(define (sum-cube a b)
    (sum a b cube inc)
)

(define (inc4 x) (+ x 4))
(define (pi-f x) (+ (/ 1.0 (* x (+ x 2)))))

(define (sum-pi a b)
    (sum a b pi-f inc4)
)

(define (integral f a b dx)
    (define (update a) (+ a dx))
    (define (init a) (+ a (/ dx 2)))
    (* dx (sum (init a) b f update))
)


;;; 1.3.2 Constructing precedures with `lambda`

(define (integral-new f a b dx)
    (* dx (sum 
            (+ a (/ dx 2.0)) 
            b 
            f 
            (lambda (x) (+ x dx))   ; update 
          )
    )
)

(define (f x y)
    (let ( ( a ( + 1 (* x y)))
           ( b ( - 1 y))
         )
     (+ (* x (* a a))
        (* y b)
        (* a b)
     )
    )
)

(define val 
    (+     (let ((x 4) (y 10)) 
       (+ x y)
           ) 
       100)
)


(define (h g) (g 2))

;;; 1.3.3