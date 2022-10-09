# Polymorphic STLC modulo isomorphism

An implementation of parsing / type-checking / type-inference for a polymorphic Simply Typed Lambda Calculus (STLC) modulo isomorphism.

_polymorphic_ here specifically means Rank 1 parametric impredicative polymorphism. Due to type annotations being mandatory, type inference is decidable and straightforward.

_modulo isomorphism_ means that we treat isomorphic functions as equal. In practical terms, this means that the order of a function's arguments do not matter. In an application, instead of checking the right side against the outermost abstraction on the left, the left side is descended into in search for any sub-abstraction that accepts the given argument. To disambiguate between different arguments of the same type, types are potentially tagged, which can be (but doesn't have to) referred to in application. In defining an abstraction, the variable's name itself is taken as its type's tag (e.g. `((\x:a.\y:a.x) $ y=0)`). Inside of a type annotation, type tags are simply annotated (`(\f:(Int -> k'Int -> Int).(f $ k=1729))`).


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
unit      : Unit
true      : Bool
false     : Bool
<numeral> : Int
add  : x'Int -> y'Int -> Int
mul  : x'Int -> y'Int -> Int
lt   : x'Int -> y'Int -> Bool
or   : x'Bool -> y'Bool -> Bool
and  : x'Bool -> y'Bool -> Bool
not  : x'Bool -> Bool
id   : x'a -> a
cond : if'Bool -> then'a -> else'a -> a
```

## Usage

The program in it's current state has no binary interface, but loading the module `STLCmodIsom` into `ghci` exposes the functions for parsing and type checking/inferring. For example:

```hs
(inferTypeIsom . parseSTLCFail) "{EXPRESSION}"
```

will parse and type-infer the expression `{EXPRESSION}`.

(Note that you have to escape the `\` in expressions as `\\` inside the Haskell String, e.g. `"\\x:a.x"`).

_Or:_

```hs
(inferTypeIsom . parseSTLCFail) <$> readFile "{FILE_PATH}"
```

will read, parse and type-infer the example code provided in the file at `{FILE_PATH}`.

For further information, see the doc comments of the exposed functions in `STLCmodIsom`.


## Examples

The final version of each example given below is provided as a file in the folder [_examples_](/examples/). You can just load an type-infer them as shown in [**Usage**](#usage) above.

Note that if you paste any example into a string in `ghci`, you have to escape the `\` in it to `\\`.

### Example 1

```julia
\x:a.x
```
`: x'a -> a`

A polymorphic identity function. The argument being named `x` means that it's type is tagged as `x'a`. We can apply it either to an argument of some type `a`:

```julia
(\x:a.(x) $ 0)
```
`: Int`

or some type `x'a`:

```julia
(\x:a.(x) $ x = 0)
```
`:Int`

This identity function `\x:a.x` is built into the language as a constant `id`.


### Example 2

```julia
\x:Int.
\y:Int.
\f:(Int -> Int).
(mul $ (f $ x) $ (f $ y))
```
`: (x'Int -> y'Int -> f'(Int->Int) -> Int)`

The above function takes two integers `x` and `y` and a unary function from integer to integer. It first applies the function to both `x` and `y`, before multiplying the results. We can give the function a name as follows:

```julia
[
    applymult :=
        \x:Int.
        \y:Int.
        \f:(Int -> Int).
        (mul $ (f $ x) $ (f $ y))
](
    applymult
)
```
`: (x'Int -> y'Int -> f'(Int->Int) -> Int)`

Now in the last pair of `()`, which currently just contains `applymult`, we can simply use the function by this label in any way we like. For example, we can partially apply it:

```julia
[
    applymult :=
        \x:Int.
        \y:Int.
        \f:(Int -> Int).
        (mul $ (f $ x) $ (f $ y))
](
    applymult $ 0
)
```
`: (y'Int -> f'(Int->Int) -> Int)`

However, what if we want to partially apply the function on it's last argument, the `Int->Int` function? That's simple, we just have to pass such a function:

```julia
[
    applymult :=
        \x:Int.
        \y:Int.
        \f:(Int -> Int).
        (mul $ (f $ x) $ (f $ y))
](
    applymult $ (add $ 1)
)
```
`: (x'Int -> y'Int -> Int)`

Notice that we did not even have to specify `f=(add $ 1)` in the application. Since there is no way for the type `Int -> Int` to be mistaken for `x'Int` or `y'Int`, it automatically is handed to the right argument `f'(Int->Int)`.

Or more formally, the language does consider the types

```julia
(x'Int -> y'Int -> f'(Int->Int) -> Int)
```

and

```julia
(f'(Int->Int) -> x'Int -> y'Int -> Int)
```

to be isomorphic to each other, so giving a function of the former type an `Int -> Int` argument means that we simply treat the function as if it had the latter type.


### Example 3

However of course many function do not have all mutually unique arguments, and so when applying them a choice has to be made as to which of it's arguments is handed a value multiple of them could take. The currently implemented approach is to simply fall back to the order in which they have been defined.

For example:

```julia
cond
```
`: (if'Bool -> then'a -> else'a -> a)`

is a constant defined in the language which simply is the equivalent to the bespoke `if ... then ... else ...` expression in other languages. However with it being an ordinary function, we can use it as such, like partially applying it:

```julia
(cond $ 0 $ 1)
```
`: (if'Bool -> Int)`

Notice that we did not have to somehow explicitly skip over the `if'Bool` argument. This is because `0` is of type `Int` and therefore can not be assigned to a type `if'Bool`, so the type checker will find a different argument which can take an `Int` instead of just throwing a typing error.\
And also notice that we did not specify whether the `then'a` or the `else'a` argument is given the `0`, and which is given the `1`. We rely on the language treating them like ordinary ordered arguments as in any ordinary language. Of course for readability, we could still explicitly specify which is `then` and which is `else`.

> **Side note:** This way to disambiguate between different possible orderings of the arguments is a perhaps natural but ultimately arbitrary choice.\
> We could just as well ensure beforehand that any two arguments of a function could never take the same value and refuse to type-check if that is the case. Or alternatively we could say that, if there is ambiguity, we assume the function to be commutative, and therefore leave it up to the compiler to make the choice for optimization.


### Example 4

So far all type annotations of arguments were just ordinary untagged types, with them simply being tagged with the name of their argument in the type of the function as a whole. However, there is a kind of situation where we need to be able to specify type tags explicitly, like:

```julia
\x:Int.
\m:(Bool -> k'Int -> a).
(m $ k = x)
```
`: x'Int -> m'(Bool -> k'Int -> a) -> Int -> a`

This means that any function `m` we wish to pass into the function would have to specifically take an argument of specifically type `k'Int` (and not e.g. `p'Int`), so that it makes sense for the function to be used in the body like `m $ k = x`. For example:

```julia
(\x:Int.
 \m:(Bool -> k'Int -> a).
 (m $ k = x)
$ (\k:Int.\q:Bool.q)
)
```
`: x'Int -> Bool -> Bool`

Note that we neither have to specify that we mean the argument `m` in the application, even though it is not the first argument to our function, nor does the function we hand in for `m` have to its arguments in the same order as specified in the type signature of `m`.

This means we are using the isomorphism of function types twice here, and in slightly different ways. For the application itself, we treat the type

```julia
x'Int -> m'(Bool -> k'Int -> a) -> Int -> a
```

as if it were

```julia
m'(Bool -> k'Int -> a) -> x'Int -> Int -> a
```

and for the argument `m`, we treat the type

```julia
k'Int -> q'Bool -> Bool
```

as if it were

```julia
Bool -> k'Int -> a
```


## Ideas

- Implement Hindley–Milner & relax type annotation requirements
- Write a more flexible parser
- Switch to using explicit multi-argument functions, since it doesn't really make sense for arguments to have a hierarchy if we treat them as mostly unordered. Question: Do we still descend into any sub-functions in search for arguments, i.e. do we allow something like `((\x:Int,y:Int. \z:Int. x) $ z=0)`?