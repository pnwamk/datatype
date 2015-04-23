# dataype
### Algebraic(-like) Data Types for Racket!

(Well... just Typed Racket at the moment...)

Big thanks to Andre Kuhlenschmidt (https://github.com/akuhlens) for patiently helping me work out the macro details!

This package should not yet be considered 'stable' -- I just finished the first implementation on 23 April and if I see something that needs fixed I may break backwards compatibility for the sake of making the feature better/correct.

### Installation & Updating
+ 'raco pkg install datatype' & 'raco pkg update datatype' from the terminal
+ or via the package manager in Dr Racket

Bug reports welcome!

### Goals

1. Construct a datatype similar to those found in ML/Haskell (i.e. **algebraic datatypes**)
2. Highly prefer using a match/case-like construct instead of 
more traditional type-predicates + accessors, and provide **static
verification** of case coverage (explicitly or with
an else)
3. Users **cannot extend** these **types** outside of their initial declaration.

### Example

Defining a new datatype:
```racket
(require datatype)

(define-datatype Expr
  [Var (Symbol)]
  [Lambda (Symbol Expr)]
  [App (Expr Expr)])
```

Using a type-case with a clause for each variant:
```racket
(: foo (Expr -> Symbol))
(define (foo e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y _) => y]
    [(App _ _) => 'app]))
```

Using a type-case with an else:

```racket
(: bar (Expr -> Symbol))
(define (bar e)
  (type-case Expr e
    [(Var x) => x]
    [(Lambda y b) => y]
    [else => (error 'foo "App not supported: ~a" e]))
```

An else clause can be used as the last clause.

Programmer must provide exactly one case for each variant XOR provide an else clause. A compile time error will warn a programmer when this requirement is not met.

### Technical Details

The example datatype expands roughly into this:

```racket
(struct Expr () #:transparent)
(struct Var ([ν : Symbol) #:transparent)
(struct Lambda ([ν : Symbol] [ν : Expr]) #:transparent)
(struct App ([ν : Expr] [ν : Expr]) #:transparent)
```

This datatype defines three struct types which all inherit from the Expr struct.

Expr structs (i.e. the empty parent class) cannot be created or extended (either will result in a run-time error when an offending instance is created).

The children structs (Var, Lambda, and App) cannot be extended (results in a run-time error when an offending instance is created).

The field names, ν, are all gensym'd to deter the usage of accessors functions.
