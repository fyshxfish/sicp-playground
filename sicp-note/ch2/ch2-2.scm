#|
Section 2.2
|#

; list 

(define l (list 5 601 3 1))
(define l2 (list 9 7 5 314))

(define (list_ref l n)
    (if ( = n 0)
        (car l)
        (list_ref (cdr l) (- n 1))
    )
)

(define (len l)
    (define (iter lst count)
        (if (null? lst)
            count 
            (iter (cdr lst) (+ count 1))
        )
    )
    (iter l 0)
)

(define (len_r l)
    (if (null? l)
        0
        (+ 1 (len_r (cdr l)))
    )
)

(define (apend lhs rhs)
    (if (null? lhs)
        rhs
        (cons (car lhs)
              (apend (cdr lhs) rhs) )
    )
)


;> E2.17

(define (last_pair l)
    (if (null? (cdr l))
        (car l)
        (last_pair (cdr l))
    )
)

;> 2.18
; HS! Pattern Match, Miss you so...
(define (revrse l)
    (define (iter lst accu)
         (if (null? lst)
             accu
             (let ((head (car lst))
                   (tail (cdr lst)))
              (iter tail (cons head accu))
             )     
         )
    )
    (iter l (list))
)

;> E 2.20
(define mod remainder)

(define (eq_bool b1 b2)
    (if (or (and b1 b2) (and (not b1) (not b2)))
        true
        false
    )
)

(define (same_parity head . tail)
    (
        let ((test (even? head)))
        (define (iter lst accu)
            (if (null? lst)
                accu
                (if (eq_bool test (even? (car lst)))
                    (iter (cdr lst) (cons (car lst) accu))
                    (iter (cdr lst) accu)
                )            
            )
        )
        (revrse (iter tail (list head)))
    )
)

(define (same_paritie hed . tal)
    (define (check_list test? lst)
        (if (null? lst)
            (list)
            (if (test? (head lst))
                (cons (head lst) (check_list test? (tail lst)))
                (check_list test? (tail lst))
            )
        )    
    )

    (let (
          (parity_test (list_ref (list even? odd?) (remainder hed 2)))  ; i like this line 
         ) 
     (cons hed (check_list parity_test tal))
    )
)


(define head car)
(define tail cdr)

(define (map proc lst)
    (if (null? lst)
        (list)
        (cons (proc (head lst))
              (map proc (tail lst))
        )
    )
)

; test
(define scale3 (lambda (x) (* x 3)))
(define l (list 3 6 10))
(define newl (map scale3 l))

(define (scale_list lst factor) ; (scale_list (list 5 7 8) 10)
    (map 
        (lambda (x) ( * x factor))
        lst)
)


;> E 2.23

(define (showme x)
    (newline)
    (display x)
)

(define (for_each proc lst)
    (if (null? lst)
        true
        (begin
            (proc (car lst))
            (for_each proc (cdr lst))
        )
    )
)

; tree

(define tree (cons (list 4 5) (list 3 7)))

(define (count_leaves t)        ; (count_leaves tree)
    (
        cond ((null? t) 0)
              ((not (pair? t)) 1)
              (else (+ (count_leaves (car t)) 
                       (count_leaves (cdr t))
                    )
              )
    )
)

(define x (list 1 2 3))
(define y (list 4 5 6))

;> E 2.27
(define (deep_reverse xss)
    (map reverse xss)
)

(define x (list (list 1 2) (list 3 4)))
(define y (deep_reverse x))

#|
;> E 2.28
(define (fringe t)
    (cond ((null? t) ())
          ((pair? t) )
          (else )
    
    )
    (define (traverse t lst)
        (cond ((null? t) ())
              ((pair? t) )
              (else (list ) )
    
        )
    )
)
|#

(define (scale_tree t factor)
    (map (lambda (sub_t)  
            (if (pair? sub_t) 
                (scale_tree sub_t factor) 
                (* sub_t factor)
            )
         ) 
    t)
)

;> E 2.30
(define (square_tree t)         ; (square_tree tree)
    (map (lambda (sub_t) 
            (if (pair? sub_t)
                (square_tree sub_t)
                (* sub_t sub_t)  
            )
         ) 
    t)
)

;> E 2.31

(define (tree_map proc t)
    (map (lambda (sub_t)
            (if (pair? sub_t)
                (tree_map proc sub_t)
                (proc sub_t)
            )
         ) 
    t)
)

(define (square x) (* x x))

(define (square_tre t) (tree_map square t))    ; NO CURRYING?

(define (sum_tree_nodes t)
    (cond ((null? t) 0)
          ((pair? t) (+ (sum_tree_nodes (car t))
                        (sum_tree_nodes (cdr t))) )
          (else t)  
    )
)

(define (filter p l)        ; test : (filter odd? (list 2 43 4 5))
    (
        cond ((null? l) ())
             ((p (car l)) (cons (car l) (filter p (cdr l))))
             (else (filter p (cdr l)))
    )
)

(define (accumulate op init l)  ; test : (accumulate + 0 (list 4 6 7 10)) ; (accumulate * 1 (list 2 3 10))
    (
        if (null? l)
           init  
           (op (car l) (accumulate op init (cdr l)))
    )
)

(define (enum_inteval low high) ; test: (enum_inteval 2 7)
    (if (> low high)
        ()
        (cons low (enum_inteval (+ low 1) high))
    )
)

(define (enum_tree_nodes t)     ; test : (enum_tree_nodes tr)
    (cond ((null? t) ())
          ((pair? t) 
                (append (enum_tree_nodes (car t)) 
                        (enum_tree_nodes (cdr t))))
          (else (list t))
    )
)

(define tr 
    (list 2 (list 1 1) (list 4 3))
)

(define (sum_odd_square tree)   ; (sum_odd_square tr)
    (accumulate + 0 (map square (filter odd? (enum_tree_nodes tree))))
)

(define (even_fibs n)   ; (even_fibs 8)
    (accumulate cons () (filter even? (map fib (enum_inteval 0 n))))
)

(define (list_fib_square n) ; (list_fib_square 10)
    (map square (map fib (enum_inteval 0 n)))
)

(define (prod_of_squared_odd sq)    ; (prod_of_squared_odd (enum_inteval 1 5))
    (accumulate * 1 (map square (filter odd? sq)))
)

(define (fib n)
    (if (or (= n 0) (= n 1))
        n
        (+ (fib (- n 1)) (fib (- n 2)))    
    )
)

(define x (list 1 2))
(define y (list 3 4))
