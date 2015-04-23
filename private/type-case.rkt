#lang racket/base

(require racket/list syntax/parse
         racket/set
         (for-template racket/base
                       racket/unsafe/ops)
         (for-syntax racket/base
                     syntax/parse))

(provide type-case parse-particular-type-case)


;;************* parse-single-case helpers *************

;; verify the case is a valid case (nominally)
(define (validate-case-name case-name case-info stx)
  (unless (hash-ref case-info case-name #f)
    (raise-syntax-error
     'type-case 
     (format "Unrecognized case. ~a\nexpected one of the following: ~a\n"
             case-name
             (hash-keys case-info))
     stx))

  (void))

;; correct binder count?
(define (validate-binders ids expected-count stx)
  (define id-count (length ids))
  ;; correct number?
  (unless (= id-count expected-count)
    (raise-syntax-error
     'type-case 
     (format "invalid number of bindings.\nexpected: ~a\ngiven: ~a\n"
             expected-count
             id-count)
     stx))
  
  ;; no-duplicates?
  (define-values (binder-ids wildcards)
    (partition (λ (x) (eqv? '_ x)) (map syntax->datum ids)))
  (define unique-var-count (length (remove-duplicates binder-ids)))
  (unless (= expected-count (+ unique-var-count (length wildcards)))
    (raise-syntax-error
     'type-case 
     "duplicate binder names not allowed.\n"
     stx))
  
  (void))
        

;;**************** individual case parser *******************

(define (parse-single-case stx arg-stx type-case-info)
  (syntax-parse stx
    [((~datum else) => exp exps ...)
     #'[else exp exps ...]]
    [((~datum else) =>)
     (raise-syntax-error 'type-case 
                         "invalid else case, missing expressions on rhs of =>\n"
                         stx)]
    [((case-id:id xs:id ...) (~datum =>) exp exps ...)
     ;; parse out some data
     (define case-name (syntax->datum #'case-id))
     (define ids (syntax->list #'(xs ...)))
     (define-values (expected-count type-predicate)
       (let ([res (hash-ref type-case-info case-name #f)])
         (if res
             (values (car res) (cadr res))
             (values #f #f))))

     ;; validate the case:
     (validate-case-name case-name type-case-info stx)
     (validate-binders ids expected-count stx)
      
     (with-syntax ([case? type-predicate]
                   [(binding ...)
                    (for/list ([id (in-list ids)]
                               [n (in-range expected-count)]
                               #:when (not (eqv? '_ (syntax->datum id))))
                      #`[#,id
                         (unsafe-struct-ref #,arg-stx #,(datum->syntax arg-stx n))])])
       #`[(case? #,arg-stx)
          (let (binding ...)
            exp exps ...)])]
    [((case-id:id xs:id ...) =>)
     (raise-syntax-error 'type-case 
                         "invalid case, missing expressions on rhs of =>\n"
                         stx)]
    [e (raise-syntax-error 'type-case 
                            "invalid case expression\n"
                            #'e)]))



;;**************** parse-particular-type-case helpers ***********

;; else-case?
;; is this case an else?
(define (else-case? stx)
  (syntax-parse stx
    [((~datum else) (~datum =>) exprs:expr ...) #t]
    [e #f]))

(define (validate-case-coverage case-exps type-case-info stx)

  ;; case-name
  ;; what is the id at the head of this case?
  (define (case-name stx)
    (syntax-parse stx
      [((case-name:id x:id ...) (~datum =>) exps ...)
       (syntax->datum #'case-name)]))

  ;; partition else vs normal cases, count them
  (define-values (else-cases type-cases)
    (partition else-case? case-exps))


  (define else-count (length else-cases))
  (define used-variants (map case-name type-cases))

  ;; determine unique vs duplicate type cases
  (define-values (unique-variants duplicates)
    (for/fold ([uniqs (seteqv)]
               [dups (seteqv)])
              ([var (in-list used-variants)])
      (cond
        [(or (set-member? uniqs var)
             (set-member? dups var))
         (values uniqs (set-add dups var))]
        [else
         (values (set-add uniqs var) dups)])))

  ;; determine if all cases are covered, and which if any
  ;; were missed
  (define-values (all-cases-covered? missed-cases)
    (for/fold ([all-cases-covered? #t]
               [missed-cases '()])
              ([case (in-hash-keys type-case-info)])
      (cond
        [(or (set-member? unique-variants case)
             (set-member? duplicates case))
         (values all-cases-covered? missed-cases)]
        [else (values #f (cons case missed-cases))])))
  ;; report errors based on all of these facts
  (cond
    ;; duplicates?
    [(not (set-empty? duplicates))
     (raise-syntax-error 'type-case 
                         (format "duplicate case(s) found:\n~a\n"
                                 (set->list duplicates))
                         stx)]
    ;; missing a case?
    [(and (= else-count 0)
          (not all-cases-covered?))
     (raise-syntax-error 'type-case 
                         (format "missing case(s) for the following:\n~a\n"
                                 missed-cases)
                         stx)]
    ;; unneeded else (i.e. cases all already covered)?
    [(and (> else-count 0)
          all-cases-covered?)
     (raise-syntax-error 'type-case 
                         "unreachable 'else' cases are not allowed\n"
                         stx)]
    ;; too many elses?
    [(> else-count 1)
     (raise-syntax-error 'type-case 
                         "there are more than 1 else cases\n"
                         #'stx)]
    ;; else case is not the last case?
    [(and (= else-count 1)
          (not (else-case? (last case-exps))))
     (raise-syntax-error 'type-case 
                         "else cases must come last\n"
                         stx)]))
;;**************** parse-particular-type-case *******************
;; builds a type case for a specific datatype

(define (parse-particular-type-case orig-stx case-stx arg-stx arg-pred type-case-info)
  (syntax-parse case-stx
    [()
     (raise-syntax-error 'type-case 
                         "cannot contain zero cases"
                         orig-stx)]
    
    [(cases:expr ...)
     (define case-exps (syntax->list #'(cases ...)))

     ;; check coverage of cases and the like
     (validate-case-coverage case-exps type-case-info orig-stx)

     (define arg-let-id (datum->syntax arg-stx (gensym 'adt)))
     ;; parse each case
     (define case-list
       (map (λ (c) (parse-single-case c arg-let-id type-case-info))
            case-exps))
     
     (if (else-case? (last case-exps))
         (with-syntax ([(cases ...)
                        case-list])
           #`(let ([#,arg-let-id #,arg-stx])
               (cond
                 cases ...)))
         (with-syntax ([(cases ...)
                        case-list])
           #`(let ([#,arg-let-id #,arg-stx])
               (cond
                 cases ...
                 [else (error 'type-case "internal type-case error: all cases missed!" #,orig-stx)]))))]))
;;**************** type-case helpers *******************
(define-for-syntax (extend-id id-stx ext-str)
  (string->symbol (string-append (symbol->string (syntax->datum id-stx))
                                                ext-str)))

;;**************** type-case *******************
;; parses a usage of type-case, expands
;; to typecase for particular type
(define-syntax (type-case stx)
  (syntax-parse stx
    [(_ type-stx:id arg-stx cases ...)
     (define id (extend-id #'type-stx "-specific-ADT-type-case"))
     (with-syntax ([spec-case (datum->syntax stx id)])
       #`(spec-case #,stx arg-stx cases ...))]))
