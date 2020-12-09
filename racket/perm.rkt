#lang racket
(provide (all-defined-out))

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