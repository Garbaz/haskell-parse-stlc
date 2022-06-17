# STLC parser

This program implements parsing for the Simply Type Lambda Calculus (STLC) with optional type annotation for the argument of an Abstraction term. For this purpose `<expr>` has the following grammar:

```
<expr>     ::= <variable> | <abstr> | <appl>
<variable> ::= <varPlain> | '(' <varPlain>  ')'
<abstr>    ::= '(\' <varAnnotated> '.' <expr> ')'
<appl>     ::= '(' <expr> '$' <expr> ')'

<varPlain>     ::= <varName>
<varAnnotated> ::= <varName> | <varName> ':' <type>
<varName>      ::= <lowercaseAlpha>

<type>         ::= <typeVar> | <functionType>
<typeVar>      ::= <capitalizedAlpha>
<functionType> ::= <type> '->' <type>
```
