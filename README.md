# Polymorphic STLC modulo isomorphism

An implementation of parsing / type-checking / type-inference for a polymorphic Simply Typed Lambda Calculus (STLC) modulo isomorphism. All abstraction arguments have to be annotated.

_polymorphic_ here is meant specifically in the sense of System F. Due to type annotations being mandatory, type inference is decidable and straightforward.

_modulo isomorphism_ means that we treat isomorphic functions as equal. In practical terms, this means that the order of a function's arguments do not matter. In an application, instead of checking the right side against the outermost abstraction on the left, the left side is descended into in search for any sub-abstraction that accepts the given argument. To disambiguate between different arguments of the same type, types are potentially tagged which can be (but doesn't have to) referred to in application. In defining an abstraction, the variables name itself is taken as its type's tag (e.g. `((\x:a.\y:a.x) $ y=0)`). Inside of a type annotation, type tags are simply annotated (`(\f:(Int -> k'Int -> Int).(f $ k=1729))`).

## Grammar

This is not quite the standard lambda calculus syntax, and has somewhat unnecessary requirements for bracketing due to the parser being written with Haskell's `ReadP` parser combinator, and not something more sensible that can handle left recursive grammars. In particular, application always has to be enclosed in brackets. However, multiple applications in a row can simply be written as a chain (Same thing with function type annotations). The `$` for application is simply because I don't want the language to be whitespace sensitive.

```ebnf

Expr ::= Const | Variable | Abstr | Appl | Let

Const    ::= unit | true | false | <numeral> | add | mul | lt | or | and | not | id | cond
Variable ::= Var | "(" Var ")"
Abstr    ::= "\" VarAnn "." Expr | "(" "\" VarAnn "." Expr ")"
Appl     ::= "(" Expr "$" Args ")"
Let      ::= "[" Var ":=" Expr "]" Expr

Args ::= Term | Term "$" Args

Term ::= Expr | TypeTag "=" Expr

Var    ::= <lowercase>
VarAnn ::= Var ":" TypeExpr

TypeTerm ::= TypeExpr | TypeTag "'" TypeExpr

TypeTag ::= <lowercase>

TypeExpr ::= TypeConst | TypeVar | TypeFunction

TypeConst    ::= Unit | Bool | Int
TypeVar      ::= <lowercase>
TypeFunction ::= "(" Froms "->" TypeExpr ")"

Froms ::= TypeTerm | TypeTerm "->" Froms
```

(Whitespace is entirely ignored)


## To-dos / open questions

### Substituting type variables

Fix a situation like `((\f:(b->b).\x:b.\y:a.(y)) $ id)`, where we end up substituting the `a` of `id` for `b`, and end up with `x'a->(y'a->a)`, i.e. our two different type variables end up becoming one.

_Idea 1_ : Do not substitute type variables like normal types from the right, but rather keep the original names and just unify them under a new name. So if we have some `f:(b->c)` and are given `id` of type `a -> a`, don't substitute the `a` in for `b` and `c`, but rather not that e.g. `c` should now become `b`. Problem: How do we propagate this? 

_Idea 2_ : Append some suffix to the type variables to ensure that they become unique. Problem: In something like `((\\f:(b->b).\\g:(c->c).\\z:c.\\x:b.\\y:a.(y)) $ id $ id)`, the `a`s of the two ids somehow have to be ensured to end up under different names.
 
_Idea 3_ : Use De Bruijn indexing.

## Ideas

- Implement Hindley–Milner & relax type annotation requirements
- Write a more flexible parser