(define (entry tree) (car tree))

(define (lhs tree) (cadr tree))

(define (rhs tree) (caddr tree))

(define (make_tree v l r) (list v l r))

(define t1 (make_tree 100 () ()))
(define t2 (make_tree 90 () ()))
(define t3 (make_tree 95 t2 t1))



(define (elem x t)
    (cond ((null? t) false)
          ((= x (entry t)) true)
          ((< x (entry t)) (elem x (lhs t)))
          ((> x (entry t)) (elem x (rhs t)))
    )
)

(define (adjoin x t)
    (cond ((null? t) (make_tree x '() '())) 
          ((= x (entry t)) t)
          ((< x (entry t)) (make_tree (entry t) (adjoin x (lhs t)) (rhs t)))
          ((> x (entry t)) (make_tree (entry t) (lhs t) (adjoin x (rhs t))))
    )
)