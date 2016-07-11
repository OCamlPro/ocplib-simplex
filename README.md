# ocplib-simplex

A library implementing a simplex algorithm, in a functional style, for
solving systems of linear inequalities


## Overview

`ocplib-simplex` is a (fully) functional OCaml implementation of the
simplex algorithm for solving systems of linear inequalities. The
implementation is incremental and backtrackable. It is able to extract
unsat-cores for unsatisfiable problems. Linear optimization is
currently not supported.


## Dependencies

`ocplib-simplex` requires `4.01.0` or higher and `ocamlfind`.
You can use `make opam-deps` to install dependencies in the current switch.


## Build and Install Instructions

Use the following instructions:

    $ autoconf (if configure is not present)
    $ ./configure
    $ make opam-deps (if you are using OPAM and some deps are missing)
    $ make
    $ make install

to compile and install `ocplib-simplex` on your system. You can
uninstall the library with `make uninstall`.


## Minimal Example

See the file `tests/standalone_minimal.ml`.


## Contributing

Don't hesitate to report encountered bugs on this Git repo's issues
tracker.


## Licensing

`ocplib-simplex` is Copyright (C) --- OCamlPro.  it is distributed
under the terms of the GNU Lesser General Public License (LGPL)
version 2.1 (see LICENSE file), with some exceptions as defined for
the OCaml Core System (see
https://github.com/ocaml/ocaml/blob/trunk/LICENSE).