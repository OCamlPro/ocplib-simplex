first public 0.1, July 11, 2016
===============================

* A functor called `Basic` provides three modules:

  - `Core`: provides some basic functions, and a function `empty` to
    create an empty environment

  - `Assert`: exports two functions `var` and `polys` to assert bounds
    on variables and polynomials, respectively

  - `Solve`: exports a function `solve` that tries to find a solution for
    the constrains

* two flags can be set when creating an empty environment to activate
  debug mode and some invariants verification

* implementation is fully functional, incremental and backtrackable

* linear optimization is not supported yet
