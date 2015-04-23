#lang typed/racket

(require "parse-type.rkt")

(define-datatype Expr
  [Var (Symbol)]
  [Lambda (Symbol Expr)]
  [App (Expr Expr)])

(: foo (Expr -> Symbol))
(define (foo e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y b) => y]
    [(App l r) => 'fool]))

