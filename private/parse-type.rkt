#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     unstable/sequence
                     racket/list)
         (for-template racket/base
                       syntax/parse))

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
                          "variant type name already bound"
                          var-id)))
  
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
    (when (> (count var-sym var-symbols) 1)
      (raise-syntax-error 'define-datatype 
                          "duplicate variant names are not allowed"
                          var-id))))

(define-for-syntax (build-class-guard class-symbol var-symbols)
       #`(λ (i)
           (when (eq? #,class-symbol i)
             (error #,class-symbol
                    "cannot construct base of a datatype ~a"
                    i))
           (unless (memq i #,var-symbols)
             (error #,class-symbol "cannot extend an algebraic data type\noffending type: ~a" i))
           (values)))

(define-for-syntax (build-varient-defs
                    var-ids
                    var-symbols
                    fields-list
                    class-id)

  (define field-defs
    (for/list ([var-id (in-list var-symbols)]
               [type-list (in-list fields-list)])
      (for/list ([type (in-list type-list)])
        #`[#,(gensym var-id) : #,type])))

  (for/list ([var-id (in-list var-ids)])
    #`(struct #,var-id #,class-id #,field-defs #:transparent)))

(define-for-syntax (extend-id id-stx ext-str)
  (datum->syntax id-stx
                 (string->symbol (string-append (symbol->string (syntax->datum id-stx))
                                                ext-str))))

(define (extend-id id-stx ext-str)
  (datum->syntax id-stx
              (string->symbol (string-append 
                               (symbol->string (syntax->datum id-stx))
                               ext-str))))

(define-for-syntax (build-class-info-hash-def var-ids var-symbols fields-list stx)
  #`(define type-info-hash
      (for/hash ([var-id (in-list var-ids)]
                 [var-sym (in-list var-symbols)]
                 [field-count (in-list (map length fields-list))])
        (values var-sym (list field-count #`#,(extend-id var-sym "?" var-id))))))

(define-for-syntax (build-specialized-type-case-def
                    type-case-id
                    class-id
                    class-info-hash-def)
  #`(define-syntax (#,type-case-id stx)
      #,class-info-hash-def
      (syntax-case stx ()
        [(_ orig-stx arg-stx cases (... ...))
         (parse-particular-type-case
          (syntax/loc #'orig-stx #'orig-stx)
          #'(cases (... ...))
          #'(ann arg-stx class-id)
          #'#,(extend-id class-id "?")
          type-info-hash)])))

;;******************* define-datatype **************

(define-syntax (define-datatype stx)
  (syntax-parse stx
    [(_ class-id:id [variants:id fieldss] ...)
     ;; convert class-id to symbol
     (define class-symbol (syntax->datum #'class-id))
     ;; grab variant identifiers and related symbol
     (define-values (var-ids var-symbols)
       (for/lists (l1 l2)
         ([var-id (in-syntax #'(variants ...))])
         (values var-id (syntax->datum var-id))))
     
     (define fields-list (map syntax->list (syntax->list #'(fieldss ...))))
     
     ;; verify identifiers are correct
     (validate-identifiers #'class-id class-symbol var-ids var-symbols)
     ;; build guard for parent type
     (define class-guard (build-class-guard class-symbol var-symbols))
     
     ;; build parent struct definition
     (define class-struct-def #`(struct #,class-symbol ()
                                  #:transparent
                                  #:guard #,class-guard))
     ;; build defs for each variant's struct
     (define variant-defs
       (build-varient-defs var-ids var-symbols fields-list #'class-id))

     ;; build type-info-hash definition
     (define class-info-hash-def
       (build-class-info-hash-def var-ids var-symbols fields-list stx))

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




