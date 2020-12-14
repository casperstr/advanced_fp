#lang racket
(provide (all-defined-out))
(require rackunit)

(define/match (isStackPermutation inp out stack)
  [('() out stack) (equal? out stack)]
  [(inp y '()) (isStackPermutation (rest inp) y (list (first inp)))]
  [(inp out stack) 
    (
      if (equal? (first out) (first stack)) 
        (isStackPermutation inp (rest out) (rest stack)) 
        (isStackPermutation (rest inp) out (cons (first inp) stack)
      )
    )
  ])

(define (perm inp out) (isStackPermutation (reverse inp) (reverse out) '()))


(define (insert-231-pattern l)
  (let-values ([(left right) (split-at l (exact-truncate (* (length l) (random))))]) 
    (let ([m (exact-floor (* (random) 1000))])
      (let ([l (exact-floor (* (random) (- m 1)))])
        (append left (list l) (list m) (list (exact-floor (* (random) (- l 1)))) right)
      ) 
    )
  )
)


(define tests
    (test-suite "Tests" 
        (test-case "Same input & output should return #t"
            (let ([numbers '(1 2 3 4 5)]) 
                (for-each 
                    (lambda (seq) 
                        (check-equal? 
                            (perm seq seq)
                            #t
                        )
                    ) 
                (permutations numbers))
            )
        )
        (test-case "231 - patterns should never be stack sortable"
            (let ([numbers '(1 2 3 4 5)]) 
                (for-each 
                    (lambda (seq) 
                        (let ([s (insert-231-pattern seq)])
                          (check-equal? 
                              (perm (sort s <) s)
                              #f
                          )
                        )
                    ) 
                (permutations numbers))
            )
        )

        (test-case "sample sequences"
          (check-equal? (perm '(1, 2, 3, 4, 5, 6) '(1, 2, 4, 5, 3, 6)) #f)
          (check-equal? (perm '(1, 2, 3, 4, 5, 6) '(2, 3, 1, 4, 5, 6)) #f)
          (check-equal? (perm '(1, 2, 3, 4, 5, 6) '(2, 5, 6, 3, 4, 1)) #f)
          (check-equal? (perm '(1, 2, 3, 4, 5, 6) '(3, 1, 2, 5, 4, 6)) #f)
          (check-equal? (perm '(1, 2, 3, 4, 5, 6) '(1, 2, 3, 5)) #f)
          (check-equal? (perm '() '()) #t)
          (check-equal? (perm '(1) '(1)) #t)
          (check-equal? (perm '(1 2 3) '(3 1 2)) #t)
        )
    )
)