# mmdoc

A simplistic parser for MetaModelica written in Haskell with the
purpose of retrieving the necessary information for generating
documentation.

It only parses a subset of MetaModelica, and hence a subset of
Modelica and does not handle non-functional parts. The result is a
simplified AST.

Do not try to use this parser to check the validity of your
program. It is meant to run only on source files that `omc` accepts.

## Notable omissions

* `matchcontinue` statements, they are not tail recursive

* Loops, use recursion

* Type variables in uniontypes and records of the form `uniontype U<A>`
  or `record R<A>`. Using these with concrete types are problematic.
  Use `replaceable type`s instead.

* `model` and `class`, use functions only

* `equation`, use `guard` or `if` statements/expressions

* Statements and expressions are parsed, but only the bare minimum to
  proceed with the rest of the parsing so their representation may not
  be very useful.

## TODO

### Parsing

* `if` expressions

* `match` guards

* `public` keyword

* Warn unless a function is declared `public` or `private`

* Warn if a package isn't encapsulated

* Warn on unprotected imports that don't explicitly import using `{}`

* Better identification of documentation strings

* `constant` definitions

* Identify

### Doc Generation

The work on this has not started yet. The output will be browsable
HTML with links between functions. An intermediary format may be used.
