# STLC parser

This program implements parsing for the Simply Type Lambda Calculus (STLC) with optional type annotation for the argument of an Abstraction term. For this purpose `<expr>` has the following grammar:

```
<expr> :== <variable> | <abstr> | <appl>
<variable> :== <varPlain> | '(' <varPlain>  ')'
<abstr> :== '(\' <varAnnotated> '.' <expr> ')'
<appl> :== '(' <expr> '$' <expr> ')'

<varPlain> := <lowercase>
<varAnnotated> := <varName> | <varName> ':' <type>

<type> :== <typeVar> | <functionType>
<typeVar> :== <capitalized>
<functionType> :== <type> '->' <type>
```

## TODO

- Allow for whitespace