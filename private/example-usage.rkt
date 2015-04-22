#lang typed/racket

(require "a.rkt")

(: foo (Expr -> Symbol))
(define (foo e)
  (Exp-ADT-type-case e
    [(Var x z) => x]
    [(Lambda y b) => y]
    ;[(App rator rand) => 'app]
    [else => 'foo]))

