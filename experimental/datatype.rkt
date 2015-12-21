#lang typed/racket

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/set))

(provide (all-defined-out))

(begin-for-syntax

  ;; validate define-datatype clause
  (define (validate-define-datatype-ids ty-id variant-ids)
    ;; is the type name free?
    (when (identifier-binding ty-id)
      (raise-syntax-error 'define-datatype 
                          "type name already bound"
                          ty-id))
    (define ty-sym (syntax->datum ty-id))
    ;; check for duplicates/collisions/etc
    (set->list
     (for/fold ([var-set (set)])
               ([var-id (in-list (syntax->list variant-ids))])
       (define var-sym (syntax->datum var-id))
       (when (eq? '_ var-sym)
         (raise-syntax-error 'define-datatype 
                             "variant name 'else' not allowed"
                             var-id))
       (when (eq? ty-sym var-sym)
         (raise-syntax-error 'define-datatype 
                             "variant name cannot equal a type name"
                             var-id))
       (when (set-member? var-set var-sym)
         (raise-syntax-error
          'define-datatype 
          "duplicate variant names are not allowed"
          var-id))
       (when (identifier-binding var-id)
         (raise-syntax-error 'define-datatype 
                             (format "variant type ~a already bound" var-sym)
                             var-id))
       (set-add var-set var-sym))))
  
  ;; validate coverage / no dupes for type-case usages
  (define (validate-type-case form-name use-site-stx variant-syms case-names last-case)
    (define variants (list->set variant-syms))
    (define case-ids (syntax->list case-names))
    (define case-syms (map syntax->datum case-ids))
    (define last-case-sym (syntax->datum last-case))

    (define-values (remaining-variants _)
      (for/fold ([remaining variants]
                 [seen-so-far (set)])
        ([ident (in-list case-ids)]
         [sym (in-list case-syms)])
        (when (set-member? seen-so-far sym)
          (raise-syntax-error form-name
                              (format "variant ~a appears more than once" sym)
                              ident))
        (unless (set-member? remaining sym)
          (raise-syntax-error form-name
                              (format "unknown variant: ~a" sym)
                              ident))
        (values (set-remove remaining sym)
                (set-add seen-so-far sym))))
    (cond
      [(eq? 'else last-case-sym) (void)]
      [else
       (unless (set-member? remaining-variants last-case-sym)
         (raise-syntax-error form-name
                             (format "unknown variant: ~a" last-case-sym)
                             last-case))
       (define leftovers (set-remove remaining-variants last-case-sym))
       (unless (set-empty? leftovers)
         (raise-syntax-error form-name
                             (format "missing cases: ~a" (set->list leftovers))
                             use-site-stx))]))
  
  ;; syntax class for variant definitions
  (define-syntax-class (type-variant type-vars)
    #:transparent
    #:attributes (name def)
    (pattern [var:id (flds ...)]
             #:with name #'var
             #:with def (with-syntax ([tvs type-vars]
                                      [(fld-ids ...)
                                       (generate-temporaries #'(flds ...))])
                          #'(struct: tvs name ([fld-ids : flds] ...)
                              #:transparent)))))

(define-syntax (define-datatype stx)
  (syntax-parse stx
    [(_ (~or (~and ty-name:id (~bind [ty-params #'()]))
             (~and (ty-name:id x:id xs:id ...)
                   (~bind [ty-params #'(x xs ...)])))
        (~var vars (type-variant #'ty-params)) ...)
     ;; define symbol versions of idents and verify idents
     (define var-syms (validate-define-datatype-ids #'ty-name #'(vars.name ...)))
     (define ty-sym (syntax->datum #'ty-name))
     (define no-ty-vars? (null? (syntax->datum #'ty-params)))
     ;; build definitions and case macro
     (with-syntax
         ([pred? (format-id stx "~a?" ty-sym)]
          [ty-case (format-id stx "~a-case" ty-sym)]
          [variant-defs #'(begin vars.def ...)]
          [union (if no-ty-vars?
                     #'(U vars.name ...)
                     #'(U (vars.name . ty-params) ...))]
          [var-syms #''(vars.name ...)]
          [predicate-def
           (if no-ty-vars?
               #'(begin (define-predicate pred? ty-name))
               #'(begin))])
       #`(...
          (begin
            variant-defs
            (define-type (ty-name . ty-params) union)
            ;; if the type is not-polymorphic, generate a default predicate
            ;; for it
            predicate-def
            (define-syntax (ty-case cstx)
              (syntax-parse cstx
                [(_ (~optional [#:inst . types-args]
                               #:defaults ([types-args #'()]))
                    val:expr
                    [(case-name flds:id ...) (~datum =>) exprs ...]
                    ...
                    (~or
                     (~and [(last-case-name last-flds ...)
                            (~datum =>) last-exprs:expr ...]
                           (~bind [last-case #'last-case-name]
                                  [last-clause
                                   #'[(last-case-name last-flds ...)
                                      last-exprs ...]]))
                     (~and [(~datum else) (~datum =>) last-exprs:expr ...]
                           (~bind [last-case #f]
                                  [last-clause #'(_ last-exprs ...)]))))
                 (when (and #,no-ty-vars? (not (null? (syntax->datum #'types-args))))
                   (raise-syntax-error #f
                                       (format "only polymorphic types can have type arguments")
                                       cstx))
                 (validate-type-case (syntax->datum #'ty-case) #'cstx var-syms #'(case-name ...) #'last-case)
                 #`(match (ann val #,(if #,no-ty-vars?
                                         #'ty-name
                                         #'(ty-name . types-args)))
                     [(case-name flds ...) exprs ...]
                     ...
                     last-clause)])))))]))



