# STLC parser

This program implements parsing for the Simply Type Lambda Calculus (STLC). For this purpose a term `Expr` has the following grammar:

```
Expr ::= Const | Variable | Abstr | Appl | Cond

Const    ::= unit | true | false | <numeral>
Variable ::= VarPlain | '(' VarPlain ')'
Abstr    ::= '(\' Var ':' Type '.' Expr ')'
Appl     ::= '(' Expr '$' Expr ')'
Cond     ::= '(' Expr '?' Expr '::' Expr ')'

Var := <lowercase>

Type         ::= TypeConst | TypeVar | FunctionType

TypeConst    ::= Unit | Bool | Int
TypeVar      ::= <capitalized>
FunctionType ::= '(' Type '->' Type ')'
```

(Whitespace is entirely ignored)
