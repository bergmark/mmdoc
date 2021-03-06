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

* Loops, use recursion

* Type variables in uniontypes and records of the form `uniontype U<A>`
  or `record R<A>`. Using these with concrete types are problematic.
  Use `replaceable type`s instead.

* `model` and `class`, use functions only

* Statements and expressions are parsed, but only the bare minimum to
  proceed with the rest of the parsing so their representation may not
  be very useful.

## TODO

### Parsing

* `match` guards

* Better identification of documentation strings
