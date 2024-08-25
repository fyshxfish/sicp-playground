(define (abs x)
  ( cond ((> x 0) x)
         ((< x 0) (- x))
         ((= x 0) 0)        
  )
)

(define (abss x )
    (cond (( > x 0 )x )
          (else (- x))
    )
)

(define (absss x)
    (if (< x 0) 
        (- x) 
        x
    )
)


; (define (:- f x)) 不能定义出和Haskell中一样的-:的原因是, lisp中的函数调用都是前缀调用
; reason why we cannot define `-:` like in Haskell: lisp's syntax of prefix functional application  

(define (square x) (* x x))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
    (if (good-enough? guess x) 
        guess 
        (sqrt-iter (improve guess x) x)    
    )
)

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

(define (improve guess x) 
    (average guess (/ x guess))
)

(define (average x y)
    (/ (+ x y) 2)
)