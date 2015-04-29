
## Building ADTs in Racket

There were two main challenges while implementing this feature
in (Typed) Racket.


### How to represent ADT values

I needed an efficient way to represent ADTs at runtime that
provided abstraction, had a tractable typing mechanism, and
allowed for the desired invariants to be enforced. Specifically,
I wanted to support the following features: a 'match' form (which
we call 'type-case') that ensures coverage while also supporting
Racket's 'match'
(in racket/match); prevented extensions to the ADT outside of
the initial type declaration.

After some experimenting and deeping reading to get the specifics
in what features were supported both in Racket and Typed Racket,
I settled on representing the ADTs with structs.

To ensure they all were subtypes of the parent type I inherited
from an empty base struct type.

To enforce creation/extension invariants, I introduced guards on
the ADT types (an undocumented feature in Typed Racket at the
moment). A runtime error is thrown if a user tries to create an
instance of the parent struct that represents the ADT type. A
runtime error is also thrown if a user ever creates an instance a
new struct type inheriting any of the original types in the ADT.
I initially was seeking to enforce these restrictions statically,
but could not identify features that could support this
invariant.  In the future I will exclude the parent struct type
and instead create a union type and type predicate (this should
incur fewer runtime costs potentially at a small cost in
typechecking time), but for the moment this struct inheritance
model works fine.

These ADTs are created with a nice concise macro:

```racket
(require datatype)

(define-datatype Expr
  [Var (Symbol)]
  [Lambda (Symbol Expr)]
  [App (Expr Expr)])
```

That expands roughly into this at the moment:

```racket
(struct Expr () #:transparent
        #:guard (λ (...) <error-if-literal-Expr-struct>))
(struct Var Expr ([ν : Symbol]) #:transparent
        #:guard (λ (...) <error-if-not-literal-Var-struct>)))
(struct Lambda Expr ([ν : Symbol] [ν : Expr]) #:transparent
        #:guard (λ (...) <error-if-not-literal-Lambda-struct>)))
(struct App Expr ([ν : Expr] [ν : Expr]) #:transparent
        #:guard (λ (...) <error-if-not-literal-App-struct>))
```

In the future it will expand into this:


```racket
(struct Var ([ν : Symbol]) #:transparent
        #:guard (λ (...) <error-if-not-literal-Var-struct>)))
(struct Lambda ([ν : Symbol] [ν : Expr]) #:transparent
        #:guard (λ (...) <error-if-not-literal-Lambda-struct>)))
(struct App ([ν : Expr] [ν : Expr]) #:transparent
        #:guard (λ (...) <error-if-not-literal-App-struct>))
(define-type Expr (U Var Lambda App))
(: Expr? (Any -> Boolean : Expr))
(define (Expr? x)
  (or (Var? x) (Lambda? x) (App? x)))
```

It also generates a hash-table that contains information about this ADT
which is passed to a 'type-case' macro generating macro.

### How to support a 'match' for ADTs

After discovering how to represent the ADT values, I moved on to
building a set of macros which could take info about an ADT's definition
and generate suport for matching on it's forms via a type-case:

```racket
(: foo (Expr -> Symbol))
(define (foo e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y _) => y]
    [(App _ _) => 'app]))
```

This was quite an exercise for me. Not only did my macro have to generate
the correct code, but it had to properly juggle syntax location information
and meta levels so all identifiers and forms it expanded into were bound
in the module it was being used in. This took quite a bit of experimenting
and learning on my part (it became easier as I started to figure out the patterns
that made this possible). It also made errors in the usage of type-case very useful!

Using type-case checks all the expected invariants: no duplicate binders, support for
wildcards _ in place of identifiers, complete coverage of cases, no reduntant cases,
and provides very detailed error messages which (in DrRacket at least) highlight the
offending fragment of code when erroring.

type-case also expands into code that is easy for Typed Racket to typecheck, and with
slight modification will work soundly in Racket as well. (Typed Racket's version can
omit some type tests which would be necc. in Racket to ensure a correct, sound typecase).

For more information on the macro specifics you can explore
parse-type.rkt and type-case.rkt.

Also, DrRacket is *amazing* for working through defining a comlex macro.
