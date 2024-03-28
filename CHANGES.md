## unreleased

## v0.5.1 (2024-03-28)
* Add documentation for solving system (PR #16).
* Separate types for coefficents and values (PR #17).
* Remove the dependency on `num` (PR #19).
* Remove messages at the `App` level (PR #22).

## 0.4.1 (2023-04-21)
* Fix the issue 13 about strict formats (PR #18).

## 0.5 (2022-11-15)
* Reworking the library build system, now only relying on dune.
  The Makefile is now clearer and simpler to use.
* Logs are handled by the `logs` library and debug is activated by this
  library.
* The `Rat2` module now abstract bounds as strict upper, strict lower or
  soft bounds instead of pairs of rationals.

## 0.4 (2017-08-22)
* Now, asserting bounds returns whether these bounds are
  trivially implied by those that are already known
* Add a field nb_pivots in the environment to count the number of
  pivots that have been made so far.

## 0.3 (2016-11-09)
* Bugfix in maximization

## 0.2 (2016-08-24)
* Add support for linear optimization (!!!). An minimal example is given
  in tests/standalone_minimal_maximization.ml
* Some bugfixes when assuming inconsistent bounds
* Improve build and testing

## 0.1 (2016-07-11)
* A functor called `Basic` provides three modules:
  - `Core`: provides some basic functions, and a function `empty` to
    create an empty environment
  - `Assert`: exports two functions `var` and `polys` to assert bounds
    on variables and polynomials, respectively
  - `Solve`: exports a function `solve` that tries to find a solution for
    the constrains
* Two flags can be set when creating an empty environment to activate
  debug mode and some invariants verification
* Implementation is fully functional, incremental and backtrackable
* Linear optimization is not supported yet
