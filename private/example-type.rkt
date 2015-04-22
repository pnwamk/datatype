#lang typed/racket

(require (for-template "type-case.rkt")
         (for-syntax "type-case.rkt")
         "type-case.rkt")

(provide (all-defined-out)
         type-case
         Expr-ADT-type-case)

#|
(define-datatype Exp
  [Var Symbol]
  [Lambda Symbol Exp]
  [App Exp Exp])
|#


(struct Expr () #:transparent
  #:guard (λ (i)
            (when (eq? 'Expr i)
              (error 'Expr "cannot construct base of a datatype ~a" i))
            (unless (memq i '(Var Lambda App))
              (error 'Expr "cannot extend an algebraic data type\noffending type: ~a" i))
            (values)))
(struct Var Expr ([x : Symbol]) #:transparent)
(struct Lambda Expr ([x : Symbol] [b : Expr]) #:transparent)
(struct App Expr ([rator : Expr] [rand : Expr]) #:transparent)

(define-syntax (Expr-ADT-type-case stx)
  (syntax-case stx ()
    [(_ orig-stx arg-stx cases ...)
     (with-syntax ([result (parse-particular-type-case
                            (syntax/loc #'orig-stx #'orig-stx)
                            #'(cases ...)
                            #'(ann arg-stx Expr)
                            #'Expr?
                            (hash 'Var (list 1 #'Var?) 
                                  'Lambda (list 2 #'Lambda?) 
                                  'App (list 2 #'App?)))])
       #'result)]))

#|
(parse-particular-type-case
      stx
      #'(cases ...)
      #'arg-stx
      #'Expr?
      (hash 'Var (list 1 #'Var?) 
            'Lambda (list 2 #'Lambda?) 
            'App (list 2 #'App?)))
|#