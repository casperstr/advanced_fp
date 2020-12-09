#lang racket
(provide (all-defined-out))




(define (neighbors graph label) 
    (second (findf (lambda (node) (equal? (first node) label)) graph))       
)           
(define (neighbors-index graph label)
   (map (lambda (l) (index-where graph (lambda(node) (equal? l (first node))))) (neighbors graph label))
)
(define (is-safe-node graph colors label color) 
   (not (member color (map (lambda (index) (list-ref colors index)) (neighbors-index graph label))))
)
(define (is-safe-graph graph colors)
    (let ([ziped (map list graph colors)])    
        (andmap (lambda (v) (is-safe-node graph colors (first (first v)) (second v) )) ziped))

)

(define (combinations xs k)
  (cond [(= k 0)     '(())]
        [(empty? xs) '()]
        [(append (combinations (rest xs) k)
                 (map (lambda(x) (cons (first xs) x))
                      (combinations xs (- k 1))))]))

(define (n-colors n) 
(if (= n 0) '() (cons n (n-colors (- n 1)) )))

(define (all-colors graph-size num-colors)  (combinations (n-colors num-colors) graph-size))

(define (find-safe-color graph num-colors) 
    (findf (lambda (colorset) (is-safe-graph graph colorset)) (all-colors (length graph) num-colors))
)

(define (k-color graph numOfColors) 
    (let ([color (find-safe-color graph numOfColors)]) 
    (if color 
        (map (lambda(node color) (list (first node) (string (integer->char (+ 96 color))))) graph color) 
        #f)
    )  
)
(k-color '((1 (2 3)) (2 (1 3)) (3 (1 2))) 3)