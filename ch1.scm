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

