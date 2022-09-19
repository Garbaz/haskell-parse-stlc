# Polymorphic STLC modulo isomorphism

An implementation of parsing / type-checking / type-inference for a polymorphic Simply Typed Lambda Calculus (STLC) modulo isomorphism.

_polymorphic_ here is meant specifically in the sense of System F. Due to type annotations being mandatory however, type inference is decidable and straightforward.

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

((Whitespace is entirely ignored in all terms))
```

### Types of constants

```julia
unit : Unit
true  : Bool
false : Bool
<numeral> : Int
add : x'Int -> y'Int -> Int
mul : x'Int -> y'Int -> Int
lt : x'Int -> y'Int -> Bool
or  : x'Bool -> y'Bool -> Bool
and : x'Bool -> y'Bool -> Bool
not : x'Bool -> Bool
id  : x'a -> a 
cond : if'Bool -> then'a -> else'a -> a
```


## To-dos / open questions

## Ideas

- Implement Hindley–Milner & relax type annotation requirements
- Write a more flexible parser
- Switch to using explicit multi-argument functions, since it doesn't really make sense for arguments to have a hierarchy if we treat them as mostly unordered. Question: Do we still descend into any sub-functions in search for arguments, i.e. do we allow something like `((\x:Int,y:Int. \z:Int. x) $ z=0)`?
