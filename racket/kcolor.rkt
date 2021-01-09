#lang racket
(provide (all-defined-out))
(require rackunit)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
 

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

(define (all-colors graph-size num-colors)  
    (combinations 
        (n-colors num-colors) 
        graph-size
    )
)

(define (find-safe-color graph num-colors) 
    (findf 
        (λ(colorset) 
            (is-safe-graph graph colorset)
        ) 
        (all-colors (length graph) num-colors))
)

(define (kcolor graph numOfColors) 
    (let ([color (find-safe-color graph numOfColors)]) 
        (if color 
            (map 
                (λ(node color) 
                    (list 
                        (first node) 
                        (string (integer->char (+ 96 color)))
                    )
                ) 
                graph 
                color
            ) 
            #f
        )
    )  
)


(println (eval (read) ns))

(define (fully-connected-graph n) 
    (map 
        (λ(v) 
            (list v 
                (remove v 
                    (build-list n values)
                )
            )
        ) 
        (build-list n values)
    )
)

(define (nun-fully-connected-graph n degree) 
    (map 
        (λ(v) 
            (list v 
                (take
                    (remove v 
                        (build-list n values)
                    )
                    degree
                )
            )
        ) 
        (build-list n values)
    )
)

(define (valid-answer res graph)
    (andmap 
        (lambda (x) 
            (not 
                (member
                    (last x) 
                    (map 
                        (lambda (neighborLabel) 
                            (last 
                                (findf 
                                    (lambda (node) (equal? (first node) neighborLabel))
                                    res
                                )
                            )
                         )
                        (neighbors graph (first x))
                    )
                )
            )
            
        )
        res
    )
)

(define tests
    (test-suite "Tests" 
        (test-case "No answers for fully connected graphs with to few colors"
            (check-equal? (kcolor '((1 (2 3)) (2 (1 3)) (3 (1 2))) 2) #f)
            (let ([numOfEdges '(3 4 5 6 7)]) 
                (for-each 
                    (lambda (n) 
                        (check-equal? 
                            (kcolor 
                                (fully-connected-graph n) 
                                (- n 1)
                            ) 
                            #f
                        )
                    ) 
                numOfEdges)
            )

        )
        (test-case "Failing haskell test"
            (check-not-equal? (kcolor '((-8 (-7 -4 -3 9 )) (-4 (-8 -7 5 )) (-7 (-8 -4 5 9 )) (9 (-8 -7 -3 5 )) (-3 (-8 5 9 )) (5 (-7 -4 -3 9 ))) 5) #f)
        )
        (test-case "Valid answers when we got enough colors"
            (let ([degree '(3 4 5 6 7)])
                (for-each
                    (lambda (d) 
                        (check-equal?
                            (let ([graph (nun-fully-connected-graph 10 d)])
                                (valid-answer (kcolor graph (+ d 1)) graph)
                            )
                            #t
                        )
                    )
                    degree
                )
            )
        )
    )
)