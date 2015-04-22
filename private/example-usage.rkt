#lang typed/racket

(require "example-type.rkt")

(: foo (Expr -> Symbol))
(define (foo e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y b) => y]
    [(App l r) => 'fool]))

