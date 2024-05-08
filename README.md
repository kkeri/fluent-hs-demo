# A Fluent demo interpreter written in Haskell

Fluent is a planned functional logic programming language built on a
concatenative core language. Ideally, all the high level features of
Fluent will be implemented as libraries in the core language.

This project implements a demo interpreter for the core language only.
It is not a reference implementation, but a simplified version
that demonstrates the ideas and the related programming techniques.

Everyone is encouraged to experiment with the language and to write
their own interpreter.
This document provides an introduction to the Fluent core language,
including its syntax, semantics, and algebraic properties.


## How to build and run

To build the project, you need the Haskell build tool `stack`.
If you don't have it installed, you can get it from https://docs.haskellstack.org/en/stable/README/.

To run the interpreter, use the `stack run` command in the main directory of the project.


## Concatenative programming and Fluent

Fluent is a pure concatenative programming language [1] [2]
with first-class continuations and algebraic effects [3].

A concatenative (or compositional) language is a language
whose syntax and semantics form the algebraic structure of a
[monoid](https://en.wikipedia.org/wiki/Monoid).
At the syntax level, the monoid operation is concatenation of terms.
The standard semantics interprets the terms as functions that take and return
a stack of values. The monoid operation is function composition [4].
But this is not the only possible semantics. Some concatenative languages are
defined using a rewrite semantics [5], and in the concatenative community
there seems to be an agreement that such rewriting systems are confluent.

The terms of concatenative languages are divided into two
categories, which are variously called *concatenative combinators* and
*arguments*, operators and operands, or other names, given by language designers.
In Fluent they are called *negative* and *positive terms*, respectively.
In a stack-based language, arguments "push themselves" onto the stack,
while combinators pop values from the stack and push new values
in their place.
Most concatenative languages use postfix notation.

Concatenative languages are sometimes contrasted with applicative languages.
Applicative languages are based on  the lambda calculus,
while concatenative languages are based on concatenative combinators.
It is known that there are several Turing-complete combinator bases [6].

Fluent differs from a typical concatenative language in a couple of ways.
The syntax uses prefix notation.
The semantics is a small-step operational semantics based on an abstract
machine.
The abstract machine has a stack, but it is a stack of combinators.
In this respect, Fluent is dual to stack-based languages.
Fluent programs can process infinite streams of data without dedicated
input and output primitives.

In the Fluent core language
nice algebraic properties take priority over user experience,
hence programs are more verbose than in other concatenative languages.
The only syntax sugar is the use of parentheses for lists.


## Syntax

A Fluent program is a sequence of terms. A term is a token or a list of terms.
Tokens are defined by regular expressions.

~~~
Program    ::= Term*
Term       ::= List | Token
List       ::= "(" Program ")"
Token      ::= Name | Paren | OpSym | String

Name       ::= [a-z][a-z0-9]*
Paren      ::= [()]
OpSym      ::= [+-*=!?/\\|<>$@#%^&~:]+
String     ::= "[^"]*"
~~~

Some tokens have special meanings in the language, but it is not indicated
in the syntax. Including lists in the syntax seems redundant,
since lists are just a sequence of terms enclosed in parentheses.
But in the abstract syntax, lists are nested structures, and they are
treated differently from tokens.


## Interpretation of programs


### Combinators

Combinators are special tokens that can be applied to non-combinator terms.
When a combinator is applied to enough arguments, it produces a sequence
of terms, that is substituted for the combinator and its arguments.
Execution is the process of applying combinators to arguments until
no applicable combinators are left in the program.
This informal semantics is sufficient for understanding the interpretation
of Fluent programs.

The following table lists all primitive combinators.
`p` and `q` are positive terms,
`t` and `u` are general terms, and `s` and `s'` are strings.

~~~
dup p    => p p               Duplicate a term.
swap p q => q `               Swap two terms.
drop p   =>                   Drop a term.

cons p (...)   => (p ...)     Prepend a term to a list.
uncons (p ...) => p (...)     Split a list into its head and tail.

tokens "" =>                  Finish tokenizing a string.
tokens s  => p tokens s'      Split a token from a string.

lists ( => nest lists         Start parsing a list.
lists p => p lists            Pass anything else.

nest ) => ()                  Finish parsing a list.
nest p => cons p nest         Prepend all other terms to a list.

flat () => )                  Finish flattening a list.
flat (p ...) => p flat (...)  Flatten a list.

pol p  => t                   Assign polarity to a term.
pols p => pol p pols          Assign polarity to a stream of terms.

fix p => apply p (fix p)      Fixed-point combinator.

apply (p q...) => t u ...     Convert a list to a sequence of polarised terms.
apply a        => pol a       Assign polarity to a token.
~~~

The combinator base is not minimal.
Some combinators are variadic, that is, they consume or produce an arbitrary
number of terms.
Note that in `lists (` and in `nest ) => ()`, `(` and `)` are tokens,
while `()` is the empty list.


### Built-in evaluation

Fluent is an *interpreted* language, where preprocessing of source code is
interleaved with execution.
Some interpreted languages expose this feature to the user via an `eval` function.
Fluent takes this idea a step further by exposing the parser to the user.
This makes Fluent a *homoiconic* language: the syntax
of the language is a data structure that can be manipulated by the language itself.
This property facilitates metaprogramming,
and makes it easier to write programs that communicate via data structures.

To evaluate source code given as a string `s`, one has to execute the
following program.

~~~
pols lists tokens s
~~~

The `tokens` combinator converts the string into a stream of tokens.
`lists` looks for opening parentheses in the stream of tokens and replaces
them with the `nest` combinator.
`nest` collects items into a list, stopping at the closing perenthesis.
For example, `nest a b c )` is translated to `cons a cons b cons c ()`.
`lists` and `nest` parse nested lists in interplay.

`lists tokens` together act as a parser that converts a string into
abstract syntax.
The final step of evaluation is to assign polarity to terms.
It is done by the `pols` combinator, which converts abstract syntax into
an executable program.


## Towards semantics

In this section I introduce a few concepts that make the structure of
the language more regular and modular.
They will simplify the definition of the semantics and make it more general.


### Polarity of terms

It is a unique feature of Fluent that terms are polarised.
Polarity enables the definition of an evaluation strategy
independently from the concrete combinators.

As it was mentioned before, the terms of concatenative languages are informally
divided into two categories, which are called combinators and arguments.
In Fluent these categories are formalized as negative and positive terms.
In concatenative combinatory logic (CCL) [6] [7], the distinction is also
made formal as *combinators* and *quotations*.
Combinators always act on quotations and not on other combinators.
This way, arguments are syntactically separated from combinators.
This seems to be a prerequisite for confluence.

In Fluent, the only negative terms are combinators. 
In the following example the positive terms are marked with `+`
and the negative terms with `-`.

~~~
cons a cons b cons c ()
---- + ---- + ---- + ++

swap a b
---- + +

dup (cons a cons b ())
--- ++++++++++++++++++
~~~

Note that a list is a single positive term.
There is no polarisation inside lists, only at the top level.


### Encoding in CCL

Positive terms can be encoded in CCL as follows:

- Tokens are enclosed into a quotation.
- Lists are replaced by the encodings of the list elements enclosed into a quotation.
- Top level lists are enclosed into an extra quotation.

I give a few examples to illustrate the encoding of negative terms.
For those who are familiar with CCL, this encoding may seem
unusual. However consider that the encoding must be reversible.

~~~
dup [a] => [a] [a]
swap [a] [b] => [b] [a]
drop [a] =>

cons [a] [[b]] => [[a] [b]]
~~~


### Partial application

Partial application is the application of a function to fewer arguments
than it expects. In concatenative languages there is no application
operator, but combinators can be seen as functions that take arguments.
Let's denote a partial application by `<c a b ...>` where `c` is a combinator
and `a b ...` are arguments.
A partial application is a negative term.
Let's redefine some combinators using partial application.

~~~
cons a     => <cons a>
<cons a> b => (a b)

swap a     => <swap a>
<swap a> b => b a
~~~

### From operations to interactions

Partial application effectively turns n-ary combinators into
unary combinators. 
This takes us to the most important concept in Fluent: *interactions*.
An interaction is a negative term followed by a positive term.
It may occur that a combinator is followed by too few arguments,
but interactions can always be reduced.
For an efficient reduction strategy, it is enough to look for interactions
in a program.

The reduction rules for interactions can be extracted into an
*interaction function* that takes a negative term and a positive term
and returns a program.

~~~
interact : Term -> Term -> Program
~~~


## Operational semantics

Now we have all the ingredients to define a proper semantics for Fluent.


Transition rules


### Programs as processes

Laziness: output is prioritized over input.


### From execution to normalisation

We reviewed the execution of Fluent programs.
But runtime behaviour is not the only interesting aspect of programs.
We may want to know how a program is reduced during execution.
For example, the operator stack of a process cannot be directly observed,



We say that a program is a normal form if it cannot be further reduced.
More specifically, a Fluent program is a normal if there are no interactions in it.
In a normal form, a negative term is never followed by a positive term,
so that the shape of normal forms is *p1 p2 ... pn n1 n2 ... nm*,
where *p1, ..., pn* are positive terms, *n1, ..., nm* are negative terms,
*n >= 0* and *m >= 0*.

StartNorm, EndNorm

compare to normalisation by evaluation

call it "normalisation by execution" or NbEx to avoid confusion with NbE


## Algebraic effects




## Program algebra

A number of equations hold for all Fluent programs.
These equations provide a basis for reasoning about programs and for
optimizing their execution.
Some of these equations are consequences of the monoid structure of the language.
Let's denote the monoid operation with `|>` and the identity element with `1`.
The following equations hold for all programs `p`, `q`, and `r`:

~~~
Assoc:     `(p |> q) |> r = p |> (q |> r) = p |> q |> r`
IdLeft:    `1 |> p = p`
IdRight:   `p |> 1 = p`
~~~


### Failure handling

Operations can also *fail*.
For example, applying `uncons` to an empty list results in a runtime error.

The classical algebraic approach to failure is to introduce a bottom element `⊥`
into the algebraic structure to represent failed computations.
Then the following equations hold for all programs `p`:

~~~
FailLeft:  `⊥ |> p = ⊥`
FailRight: `p |> ⊥ = ⊥`
~~~

Together these equations guarantee that if an operation fails,
the whole program fails without producing a result.
I call this approach *atomic failure*.

In a distributed system, e.g. a pipeline of processes, this approach is
not sufficient.
In a pipeline, processes are connected via data channels.
A failure can be treated as a special message that is propagated through the system.
A process that receives a failure message must stop its execution
and propagate the failure further.

A failed process could have produced some output before it failed
and the output might be consumed by other processes.
It cannot be revoked after the failure.
In this case, only the `FailRight` equation holds.
It corresponds to a weaker but still useful guarantee that
a failed process does not produce any new output.
Consequently, a process that depends on a failed process, cannot proceed.
I call this approach *propagated failure*.


### Mapping between domains

Domains:

- list of characters
- list of tokens
- list of (polarised) terms
- processes

TODO:
show that:
- all domains have monoid structure
- the mappings are homomorphic
- the domains are isomorphic


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
- [3] An Introduction to Algebraic Effects and Handlers  
  https://www.eff-lang.org/handlers-tutorial.pdf
- [4] Jurij Mihelic, William Steingartner, Valerie Novitzka:
  A denotational semantics of a concatenative/compositional programming language  
  https://acta.uni-obuda.hu/Mihelic_Steingartner_Novitzka_111.pdf
- [5] The Enchilada language (Term rewriting)  
  https://web.archive.org/web/20231202225035fw_/http://enchiladacode.nl/reference.html#rewriting
- [6] Brent Kirby: The theory of concatenative combinators  
  http://tunes.org/~iepos/joy.html
- [7] Remo Dentato: The role of quotes in Concatenative Combinatory Logic
  https://hackmd.io/@qeHlwm2zQ62-hoUHOp_E5w/r11Zu4a0t