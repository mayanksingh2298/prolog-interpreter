# Prolog Interpreter
This is a basic implementation of a [Prolog](http://www.swi-prolog.org/) interpreter in [OCaml](https://ocaml.org/) with REPL support.
The interpreter follows standard Prolog IO syntax where clauses, facts, rules and terms have their usual definition.

## Implementation
The implementation consists of a lexer `lexer.mll`, a parser `parser.mly`, an REP loop `toyprolog.ml` and the core unification and substitution engine `a6.ml`.

## How To
* The project contains a **makefile** that generates an executable **toyprolog**.
* The executable can be run as ` ./toyprolog inputfile` where `inputfile` should contain all facts and rules to be loaded before execution.
* The project also contains a sample prolog script based on the [Harry Potter](https://en.wikipedia.org/wiki/Harry_Potter) universe family trees. Its called `hogwarts.pl` and contains lots of interesting facts and rules.

## An Example
```prolog
make
./toyprolog sample-prolog-codes/hogwarts.pl
?-brother(X,ginnyWeasley).
X = ronWeasley;
X = georgeWeasley;
X = fredWeasley;
X = percyWeasley;
X = charlieWeasley;
X = billWeasley;
false.

?-ancestor(X,harryPotter).
X = doreaBlack;
X = charlusPotter;
X = violettaBulstrode;
X = cygnusBlack;
X = ursulaFlint;
X = phineasNigellusBlack;
X = lillyPotter;
X = jamesPotter;
false.
```
