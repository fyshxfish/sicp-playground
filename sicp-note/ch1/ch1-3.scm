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

;; 1.3.3

;;; find zero point (half interval)

(define (avr x y)
    (/ (+ x y) 2)
)

(define (search f neg-point pos-point)
    (define (close-enough? a b)
        (< 
            (abs (- a b)) 
            0.001
        )
    )

    (let (
            (mid (avr pos-point neg-point))
         )
        (cond 
              ((close-enough? pos-point neg-point) mid)
              ((positive? (f mid)) (search f neg-point mid))
              (else (search f mid pos-point))
        )
    )
)

(define (half-interval-search f a b)
    ( let (
            (a-val (f a))
            (b-val (f b))
          )
      (cond
            ((and (negative? a-val) (positive? b-val)) (search f a b) )
            ((and (negative? b-val) (positive? a-val)) (search f b a) )
            (else (error "f(a) * f(b) > 0. params:" a b))
      )
    )
)

;;; fixed point 

(define tolerance 0.00001)

(define (search-fixed-point f x)
    (define (close-enough? v1 v2)
        ( < 
            (abs (- v1 v2))
            tolerance
        )
    )

    (newline)
    (display x)

    (if (close-enough? x (f x)) 
        x
        (search-fixed-point f (f x))
    )
)

;- test with: (search-fixed-point (lambda (x) (+ (cos x) (sin x))) 1.0)

;;; find square root with fixed point 

(define (sqrt x)
    (search-fixed-point 
        (lambda (y) (/ (+ y (/ x y)) 2.0)) ; if replace 2.0 with 2, the return value will be a fraction
        1.0
    )
)


;> E-1.35: find golden ratio by fixed-point 

(define (fi guess)          ; change guess to see how fast it is from guess to fixed point 
    (search-fixed-point 
        (lambda (x) (+ 1 (/ 1 x)))
        guess 
    )
)

;> E-1.36

(define (ans guess)
    (search-fixed-point
        (lambda (x) (/ (log 1000) (log x)))
        guess 
    )
)

(define (ans_avr guess)     ; less steps
    (search-fixed-point
        (lambda (x) (avr x (/ (log 1000) (log x))))
        guess 
    )    
)

;> E-1.37

(define (cont-frac n d k) 
    (define (inner level value)     ; closure ; iterative 
        (cond ( (= level k)
                (inner 
                    (- level 1) 
                    (/ (n k) (d k)))
              )
              ( (= level 1) 
                (/ (n 1) (+ (d 1) value))
              )
              ( else 
                (inner 
                    (- level 1)
                    (/ (n (- level 1)) (+ (d (- level 1) ) value))  
                )
              ) 
        )
    )
    (inner k 0)
)

;>- test with `(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 100)` to get approximate golden ratio


#| 
Section 1.3.4 procedures as returned values 
|#

(define (average x y)
    (/ (+ x y) 2)
)

(define (average-dump f)
    (lambda (x) (average x (f x)))
)

;- (average-dump square)

(define (cube-sqrt x)
    (search-fixed-point 
        (average-dump (lambda (y) (/ x (square y))))
        1.0
    )
)


;;; express the idea of derivative

(define dx 0.00001)

(define (deriv g)
    (lambda (x)    
       (/ (- (g (+ x dx))
           (g x) 
          )
          dx
       )
    )        
)

;test> ((deriv cube) 1)

;: newton-method: a solution of the equation g(x) = 0
;: is a fixed point of the function x -> f(x)
;: f(x) defined as below

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newton-method g guess)
    (search-fixed-point (newton-transform g) guess)
)

;> E 1.40

(define (cubic a b c)
    (lambda (x) 
            (+ 
                (* x x x)
                (* a x x)
                (* b x)
                c
            )
    )
)

(define x140
    (newton-method (cubic 1 1 0) 1.0)
)

(define (sqrt x)
    (newton-method 
        (lambda (y) (- x (square y)))
        1.0
    )
)

(define (fixed-point-transform g transform guess)
    (search-fixed-point (transform g) guess)
)

(define (sqrt x)
    (fixed-point-transform 
        (lambda (y) (/ x y))
        average-dump
        1.2
    )
)

(define (sqrt x)
    (fixed-point-transform 
        (lambda (y) (- x (square y)))
        newton-transform
        1.4
    )
)

;> E 1.41

(define (double f)
    (lambda (x)
        (f (f x))
    )
)

;test> (((double (double double)) inc) 5)

:> E 1.42
(define (compose f g)
    (lambda (x) (f (g x)))
)

;test> ((compose square inc) 6)