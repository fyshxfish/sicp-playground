(define (make_leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? node)
    (eq? (car node) 'leaf)
)

(define (symbol_leaf leaf) (cadr leaf))

(define (weight_leaf leaf) (caddr leaf))

(define (make_code_tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))      
    )
)

(define (left_branch tree) (car tree))
(define (right_branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol_leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree))
        (list (weight_leaf tree))
        (cadddr tree)
)

(define (choose_branch bit tree)
    (cond ((= 0 bit) (left_branch tree))
          ((= 1 bit) (right_branch tree))
          (else "error CHOOSE_BRANCH bit:", bit)
    )
)

;; zipper in HS?

(define (decode bits tree)
    (define (decode_cur bits current_tree)
        (if (null? bits)
            '()
            (let ((next_branch (choose_branch (car bits) current_tree)))
                (if (leaf? next_branch)
                    (cons (symbol_leaf next_branch)
                        (decode_cur (cdr bits) tree)
                    )
                    (decode_cur (cdr bits) next_branch)
                ) 
            )
        )
    )
    (decode_cur bits tree)
)

(define (adjoin_set x set)
    (cond ((null? set) (list x)) 
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin_set x (cdr set))))
    )
)

(define (make_leaf_set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
             (adjoin_set (make_leaf (car pair) (cadr pair))
                         (make_leaf_set (cdr pairs))
             )
        )
    )
)

