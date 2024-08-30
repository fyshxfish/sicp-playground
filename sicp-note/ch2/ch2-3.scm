; welcome to Section 2.3 
; wish you enjoy this meme:
;  user: say my name
;  bash: name 
;  user: I mean, say my $name 

#|
Section 2.3.1
|#

(define (memq item seq)
    (cond ((null? seq) false)
          ((eq? item (car seq)) seq)
          (else (memq item (cdr seq)))
    )
)

(define x '(this (is a) list))
(define y '(this (is a) list))

; E 2.54

(define (equal? x y) 
    (if (pair? x) 
        (and 
            (equal? (car x) (car y))    ; equal? cannot be replaced by eq?, because (car x) may be a pair
            (equal? (cdr x) (cdr y))
        )  
        (eq? x y)
    )
)

(define f (car ''qq))
(define x (f '(q w)))

