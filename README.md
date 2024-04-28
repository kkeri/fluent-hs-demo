# A Fluent demo interpreter written in Haskell

This is a demo implementation of the Fluent language.
It is not a faithful implementation of the full language, but a simplified version
that demonstrates the core ideas and the related programming techniques.
Clear mapping of concepts into source code is prioritized over performance.

This document starts with an introduction to the language and its algebraic properties,
then it describes the evaluation strategies and the normalisation process.
Finally, it gives a brief overview of the interpreter.


## Concatenative programming and Fluent

Fluent is a pure concatenative programming language [1], [2]
with first-class continuations and algebraic effects [3].

A concatenative (or compositional) language is a language
whose syntax and semantics form monoids.
At the syntax level, the monoid operation is concatenation of terms.
The standard semantics interprets the terms as functions that take and return
a stack of values. The monoid operation is function composition [4].
But this is not the only possible semantics. Some concatenative languages are
defined using a rewrite semantics [5], and in the concatenative community
there seems to be an agreement that such rewriting systems are confluent.

The terms of a concatenative language are informally divided into two
categories, which are variously called combinators and arguments, 
or operators and operands.
Operands are pushed onto the stack, while operators
pop values from the stack and push new values in their place.
Most concatenative languages use postfix notation.
Concatenative languages are sometimes contrasted with applicative languages.
Applicative languages are based on  the lambda calculus,
while concatenative languages are based on concatenative combinators.
It is known that there are several Turing-complete combinator bases [6].

Fluent differs from a typical concatenative language in a couple of ways.
The syntax uses prefix notation.
The semantics is a small-step operational semantics based on an abstract
machine.
The abstract machine has a stack, but it is a stack of operators.
In this respect, Fluent is dual to stack-based languages.
Fluent programs can process infinite streams of data without dedicated
input and output primitives.

In the Fluent core language, which is implemented by this project,
minimality and nice algebraic properties take priority over programmer experience,
hence programs are more verbose than in a typical concatenative language.


## Syntax

A Fluent program is a sequence of terms. A term is a token or a list of terms.

~~~
Program    ::= Term*

Term       ::= Combinator | Argument
Combinator ::= dup | drop | swap ...
Argument   ::= List | Token
List       ::= "[" Program "]"

Token      ::= Name | Bracket | OpSym | String
Name       ::= [a-z][a-z0-9]*
Bracket    ::= "(" | ")"
OpSym      ::= [+-*=!?/\\|<>$@#%^&~:]+
String     ::= '"' [^"]* '"'
~~~


### Combinators

When a combinator is applied to enough arguments, it produces a program,
that is, a sequence of terms.
The program is substituted for the combinator and its arguments.

In the following table, `a` and `b` are arguments, `t` and `u` are terms.

| Operator with signature         | Description                             |
|---------------------------------|-----------------------------------------|
| `cons a (...) -> (a ...)`       | Prepend a term to a list.               |
| `uncons (a ...) -> a (...)`     | Split a list into its head and tail.    |
| `dup a -> a a`                  | Duplicate a term.                       |
| `swap a b -> b a`               | Swap two terms.                         |
| `drop a ->`                     | Drop a term.                            |
| `tokens "" -> `                 | Finish tokenizing a string.             |
| `tokens s -> a tokens s'`       | Split a token from a string.            |
| `nest ( -> coll nest`           | Start parsing a list.                   |
| `nest a -> a nest`              | Pass anything else.                     |
| `coll ) -> ()`                  | End parsing a list.                     |
| `coll a -> cons a coll`         | Collect all other terms into a list.    |
| `pol a -> v pol`                | Assigns polarity to terms.              |
| `fix a -> apply a (fix a)`      | Fixed-point combinator.                 |
| `apply (a b...) -> v w ...`     | Convert a list to a sequence of values. |

Note that in `coll ) -> ()`, `)` is a token, while `()` is the empty list.
In `nest (`, `(` is also a token.

Some combinators are variadic, that is, they consume or produce an arbitrary
number of terms.

### Interpretation

Interpretation of source code is embedded into the language.
This makes Fluent a *homoiconic* language, where the syntax
of the language is a data structure that can be manipulated by the language itself.
This property facilitates metaprogramming,
and makes it easier to write programs that communicate via data structures.

To parse and execute source code given as a string `s`, one has to execute the
following program:

~~~
pol nest tokens s
~~~

`tokens` converts the string into a stream of tokens.
`nest` looks for lists in the stream of tokens and calls `coll` for each list.
`coll` collects items into a list, stopping at the closing perenthesis.
For example, `coll a b c )` is translated to `cons a cons b cons c ()`,
where `()` is the empty list.
`nest` and `coll` interplay to parse nested lists.
`pol` assigns polarities to terms, as described below.
Without `pol`, the program would be interpreted as a data stream.


## Program algebra

Some axioms hold for Fluent programs independently of the set of combinators.

|> is an associative binary operator that concatenates programs.
The identity element of |> is the empty program, which is denoted by `()` in this section.
Fail is a zero element for |>.


## Execution of a program

The first step of evaluation is to assign polarity to terms.
Polarity controls evaluation, and enables the definition of an evaluation strategy
that is independent of the meaning of the terms.
The rules for assigning polarities are simple:

- Operators are assigned negative polarity, and
- every other term is assigned positive polarity.


## Normal forms

A program is a normal if there are no interactions in it.
In other words, a negative term is never followed by a positive term,
so that the shape of normal forms is *p1 p2 ... pn n1 n2 ... nm*,
where *p1, ..., pn* are positive terms, *n1, ..., nm* are negative terms,
*n >= 0* and *m >= 0*.


## Operational semantics

Transition rules


## Programs as processes

Laziness: output is prioritized over input.


## From execution to normalisation

StartNorm, EndNorm

compare to normalisation by evaluation

call it "normalisation by execution" or NbEx to avoid confusion with NbE


## Failure propagation

Atomic failure vs. failure propagation.


## Glossary

- **Operator**: A combinator that takes one or more arguments and returns a result.
- **Operand**: An argument to an operator.
- **Prefix notation**: In the source code the operator precedes its operands.
  For example, `+ 1 2` is a prefix notation for the infix expression `1 + 2`.
- **Postfix notation**: In the source code the operator follows its operands.
  For example, `1 2 +` is a postfix notation for the infix expression `1 + 2`.
- **Term**: An operator or an operand in the source code.
- **Value**: The semantic interpretation of a term.
- **Polarized value**: A term with negative/positive polarity.
  Operators are interpreted as negative values and operands as positive values.
- **Program**: A sequence of values.
- **Interaction**: A negative value followed by a positive value in a program.
- **Interaction function**: A function that takes an operator and an operand
  and returns a program.
- **Order of evaluation**: The order in which the interactions are evaluated.

### Notes on terminology

I try to avoid *left* and *right* in my terminology.
Sidedness appers at several levels in the language:
ltr/rtl write directionality, prefix/postfix notation, ordering of redexes
and order of evaluation. The two latter are abstract dimensions, while
left and right are concrete directions that depend on the presentation.

Prefix and postfix notations are best understood in relation to the order of reading.
In prefix notation, the operator is read first, while in postfix notation,
the operator is read last.


## References

- [1] Wikipedia: Concatenative programming language
  https://en.wikipedia.org/wiki/Concatenative_programming_language
- [2] Manfred von Thun: *Joy:* Forth's Functional Cousin
  https://hypercubed.github.io/joy/html/forth-joy.html
- [3] Algebraic effects ...
- [4] Jurij Mihelic, William Steingartner, Valerie Novitzka:
  A denotational semantics of a concatenative/compositional programming language
  https://acta.uni-obuda.hu/Mihelic_Steingartner_Novitzka_111.pdf
- [5] The Enchilada language (Term rewriting)
  https://web.archive.org/web/20231202225035fw_/http://enchiladacode.nl/reference.html#rewriting
- [6] Brent Kirby: The theory of concatenative combinators
  http://tunes.org/~iepos/joy.html
