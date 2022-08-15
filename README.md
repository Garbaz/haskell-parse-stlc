# STLC modulo Isomorphism

An implementation of parsing / type-checking / type-inference for a Simply Typed Lambda Calculus (STLC) modulo isomorphism.

## Grammar

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
TypeVar      ::= <capitalized>
TypeFunction ::= "(" Froms "->" TypeExpr ")"

Froms ::= TypeTerm | TypeTerm "->" Froms
```

(Whitespace is entirely ignored)
