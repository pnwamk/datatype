#lang typed/racket/base

(require "type-case.rkt"
         (for-syntax typed/racket/base
                     syntax/parse
                     racket/list
                     "type-case.rkt")
         (for-template typed/racket/base
                       syntax/parse
                       "type-case.rkt"))



(provide (all-defined-out)
         type-case
         struct:)

#|
(define-datatype Exp
  [Var (Symbol)]
  [Lambda (Symbol Exp)]
  [App (Exp Exp)])
|#

(define-for-syntax (validate-identifiers class-id class-symbol var-ids var-symbols)

  ;; is the type name free?
  (when (identifier-binding class-id)
    (raise-syntax-error 'define-datatype 
                        "type name already bound"
                        class-id))
  ;; are all the variant type names free?
  (for ([var-id (in-list var-ids)])
    (when (identifier-binding var-id)
      (raise-syntax-error 'define-datatype 
                          (format "variant type ~a already bound" var-id)
                          class-id)))
  
  ;; verify type-name not found as a variant name
  (for ([var-sym (in-list var-symbols)]
        [var-id (in-list var-ids)])
    (when (eq? class-symbol var-sym)
      (raise-syntax-error 'define-datatype 
                          "variant name cannot equal a type name"
                          var-id)))
  ;; check for duplicate variant-type names
  (for ([var-sym (in-list var-symbols)]
        [var-id (in-list var-ids)])
    (when (> (count (λ (name) (eq? name var-sym)) var-symbols) 1)
      (raise-syntax-error 'define-datatype 
                          "duplicate variant names are not allowed"
                          var-id))))

(define-for-syntax (build-class-guard class-symbol var-symbols)
  #`(λ (i)
      (when (eq? (quote #,class-symbol) i)
        (error (quote #,class-symbol)
               "cannot construct base of a datatype ~a"
               i))
      (unless (memq i (quote #,var-symbols))
        (error (quote #,class-symbol) "cannot extend an algebraic data type\noffending type: ~a" i))
      (values)))

(define-for-syntax (build-varient-defs
                    var-ids
                    var-symbols
                    fields-list
                    class-id)

  #;(define class-id (datum->syntax (and (not (null? var-ids)) (car var-ids))
                                  class-id))
  (define field-defss
    (for/list ([var-sym (in-list var-symbols)]
               [var-id (in-list var-ids)]
               [type-list (in-list fields-list)])
      (for/list ([type (in-list type-list)])
        (datum->syntax var-id `[,(gensym var-sym) : ,type]))))

  (for/list ([var-id (in-list var-ids)]
             [field-defs (in-list field-defss)])
    (with-syntax ([(fields ...) field-defs])
      #`(struct: #,var-id #,class-id (fields ...) #:transparent))))

(define-for-syntax (extend-id id-stx ext-str)
  (datum->syntax id-stx
                 (string->symbol (string-append (symbol->string (syntax->datum id-stx))
                                                ext-str))))

(define (extend-id [id-stx : (Syntaxof Any)] [ext-str : String])
  (datum->syntax id-stx
                 (string->symbol (string-append (symbol->string (cast (syntax->datum id-stx) Symbol))
                                                ext-str))))

(define-for-syntax (build-class-info-hash-def var-ids var-symbols fields-list)
  (with-syntax ([(var-idents ...) var-ids]
                [(var-syms ...) var-symbols]
                [(fields ...) fields-list])
    #`(define type-info-hash
      (for/hash ([var-id (in-list `(,#'var-idents ...))]
                 [var-sym (in-list '(var-syms ...))]
                 [field-count (in-list (map length '(fields ...)))])
        (values var-sym (list field-count #`#,(extend-id var-id "?")))))))

(define-for-syntax (build-specialized-type-case-def
                    type-case-id
                    class-id
                    class-info-hash-def)
  (define pred? (extend-id class-id "?"))
  (define type-info-hash (datum->syntax class-info-hash-def 'type-info-hash))
  #`(define-syntax (#,type-case-id stx)
      #,class-info-hash-def
      (syntax-case stx ()
        [(_ orig-stx arg-stx cases (... ...))
         (parse-particular-type-case
          (syntax/loc #'orig-stx #'orig-stx)
          #'(cases (... ...))
          #'(ann arg-stx #,class-id)
          #'#,pred?
          #,type-info-hash)])))

;;******************* define-datatype **************

(define-syntax (define-datatype stx)
  (syntax-parse stx
    [(_ class-id:id [variants:id fieldss] ...)
     ;; convert class-id to symbol
     (define class-symbol (syntax->datum #'class-id))
     ;; grab variant identifiers and related symbol
     (define var-ids (syntax->list #'(variants ...)))
     (define var-symbols (map syntax->datum var-ids))
     (define fields-list (map syntax->list (syntax->list #'(fieldss ...))))
     
     ;; verify identifiers are correct
     (validate-identifiers #'class-id class-symbol var-ids var-symbols)
     ;; build guard for parent type
     (define class-guard (build-class-guard class-symbol var-symbols))
     
     ;; build parent struct definition
     (define class-struct-def #`(struct: class-id ()
                                  #:transparent
                                  #:guard #,class-guard))
     ;; build defs for each variant's struct
     (define variant-defs
       (build-varient-defs var-ids var-symbols fields-list #'class-id))

     ;; build type-info-hash definition
     (define class-info-hash-def
       (build-class-info-hash-def var-ids var-symbols fields-list))

     ;; define specialize name for this class's type-case
     (define type-case-id (extend-id #'class-id "-specific-ADT-type-case"))

     ;; define specialized type-case constructor
     (define specialized-type-case-def
       (build-specialized-type-case-def
        type-case-id
        #'class-id
        class-info-hash-def))
     
     (with-syntax ([(var-defs ...)
                    variant-defs])
       #`(begin
           #,class-struct-def
           var-defs ...
           #,specialized-type-case-def))]))




