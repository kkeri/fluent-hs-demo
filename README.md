# A Fluent demo interpreter written in Haskell

This is a demo interpreter for the Fluent core language.
It is not a reference implementation of the language, but a simplified version
that demonstrates the core ideas and the related programming techniques.
Clear mapping of concepts into source code is a goal of this project,
but runtime efficiency is not.

## How to build and run

To build the project, you need the Haskell build tool `stack`.
If you don't have it installed, you can get it from https://docs.haskellstack.org/en/stable/README/.

To run the interpreter, use the `stack run` command in the main directory of the project.


## Concatenative programming and Fluent

Fluent is a pure concatenative programming language [1], [2]
with first-class continuations and algebraic effects [3].

A concatenative (or compositional) language is a language
whose syntax and semantics form the algebraic structure of a monoid.
At the syntax level, the monoid operation is concatenation of terms.
The standard semantics interprets the terms as functions that take and return
a stack of values. The monoid operation is function composition [4].
But this is not the only possible semantics. Some concatenative languages are
defined using a rewrite semantics [5], and in the concatenative community
there seems to be an agreement that such rewriting systems are confluent.

The terms of a concatenative language are informally divided into two
categories, which are called combinators and arguments, 
or operators and operands. In the Fluent language they are called
negative and positive terms, respectively.
Operands "push themselves" onto the stack, while operators
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
In the signatures, `p` and `q` are positive terms, `n` and `o` are negative terms, 
`t` and `u` are general terms, and `s` and `s'` are strings.

~~~
cons p (...) -> (p ...)       Prepend a term to a list.
uncons (p ...) -> p (...)     Split a list into its head and tail.

dup p -> p p                  Duplicate a term.
swap p q -> q `               Swap two terms.
drop p ->                     Drop a term.

tokens "" ->                  Finish tokenizing a string.
tokens s -> p tokens s'       Split a token from a string.

lists ( -> coll lists         Start parsing a list.
lists p -> p lists            Pass anything else.

coll ) -> ()                  End parsing a list.
coll p -> cons p coll         Collect all other terms into a list.

pol p -> t                    Assigns polarity to a term.
pols p -> pol p pols          Assigns polarity to a stream of terms.

fix p -> apply p (fix p)      Fixed-point combinator.
apply (p q...) -> t u ...     Convert a list to a sequence of terms.
~~~

The above combinator base is not necessarily minimal.
Some combinators are variadic, that is, they consume or produce an arbitrary
number of terms.
Note that in `lists (` and in `coll ) -> ()`, `)` is a token,
while `()` is the empty list.

### Built-in evaluation

Fluent is an *interpreted* language.
The source code is executed directly, without a separate compilation step.
In other words, preprocessing of source code is interleaved with execution.
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
`lists` looks for opening parentheses in the stream of tokens and inserts
`coll` in the program for each one.
`coll` collects items into a list, stopping at the closing perenthesis.
For example, `coll a b c )` is translated to `cons a cons b cons c ()`,
where `()` is the empty list.
`lists` and `coll` parse nested lists in interplay.
`pols` assigns polarity to terms, as described below.
Without `pols`, the source code would be interpreted as a data stream
and not as a program.


### Polarity of terms

The first step of evaluation is to assign polarity to terms.
Polarity controls evaluation, and enables the definition of an evaluation strategy
that is independent of the meaning of the terms.
The rules for assigning polarities are simple:

- Operators are assigned negative polarity, and
- every other term is assigned positive polarity.


## From execution to normalisation

We reviewed the execution of Fluent programs.
But runtime behaviour is not the only interesting aspect of programs.
We may want to know how a program is reduced during execution.
For example, the operator stack of a process cannot be directly observed,



We say that a program is a normal form if it cannot be further reduced.
More specifically, a Fluent program is a normal if there are no interactions in it.
In other words, a negative term is never followed by a positive term,
so that the shape of normal forms is *p1 p2 ... pn n1 n2 ... nm*,
where *p1, ..., pn* are positive terms, *n1, ..., nm* are negative terms,
*n >= 0* and *m >= 0*.

StartNorm, EndNorm

compare to normalisation by evaluation

call it "normalisation by execution" or NbEx to avoid confusion with NbE


## Operational semantics

Transition rules


### Programs as processes

Laziness: output is prioritized over input.


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

Domains: list of characters, list of tokens, list of values, processes.

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
