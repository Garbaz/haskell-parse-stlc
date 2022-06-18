# STLC parser

This program implements parsing for the Simply Type Lambda Calculus (STLC) with optional type annotation for the argument of an Abstraction term. For this purpose a term `Expr` has the following grammar:

```vim
Expr ::= Const | Variable | Abstr | Appl | Cond

Const    ::= unit | true | false | <numeral>
Variable ::= VarPlain | '(' VarPlain ')'
Abstr    ::= '(\' VarAnnotated '.' Expr ')'
Appl     ::= '(' Expr '$' Expr ')'
Cond     ::= '(' Expr '?' Expr '::' Expr ')'

VarPlain     ::= VarName
VarAnnotated ::= VarName | VarName ':' Type
VarName      ::= <lowercase>

Type         ::= TypeConst | TypeVar | FunctionType

TypeConst    ::= Unit | Bool | Int
TypeVar      ::= <capitalized>
FunctionType ::= '(' Type '->' Type ')'
```

(Whitespace is entirely ignored)
