#lang typed/racket

(require "datatype.rkt")
(define-type Int Integer)

(define-datatype (Opt A)
  [SOME (A)]
  [NONE ()])

(define (extract-int [x : (Opt Int)]) : Integer
  (Opt-case [#:inst Int]
   x
   [(SOME i) => i]
   [(NONE) => -1]))

(define-datatype Expr
  [Var (Symbol)]
  [Lambda (Symbol Expr)]
  [App (Expr Expr)])

(: foo (Expr -> Symbol))
(define (foo e)
  (Expr-case
   e
   [(Var x) => x]
   [(Lambda y _) => y]
   [(App _ _) => 'app]))
