#lang typed/racket

(require "parse-type.rkt")

(define-datatype Expr
  [Var ([n : Symbol])]
  [Lambda (Symbol Expr)]
  [App (Expr (Boxof Expr))])

(: foo (Expr -> Symbol))
(define (foo e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y b) => y]
    [(App l r) => 'fool]))


(Var-n (Var 'hello))


(define-datatype AAAA
  #:property prop:custom-write (lambda (me port mode)
                                 (eprintf "10"))
  [Hi ()])

