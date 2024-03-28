(lang dune 3.0)
(generate_opam_files true)

(name ocplib-simplex)
(version dev)
(source (github OCamlPro/ocplib-simplex))
(authors
  "Mohamed Iguernlala <iguer@functori.com>"
  "Hichem Ait El Hara <hichem.ait-el-hara@ocamlpro.com>"
  "Steven de Oliveira <steven.de-oliveira@ocamlpro.com>"
  "Guillaume Bury <guillaume.bury@ocamlpro.com>"
  "Pierre Villemot <pierre.villemot@ocamlpro.com>"
)
(maintainers "OCamlPro <contact@ocamlpro.com>")
(documentation "https://ocamlpro.github.io/ocplib-simplex")

(package
 (name ocplib-simplex)
 (synopsis
  "A library implementing a simplex algorithm, in a functional style, for solving systems of linear inequalities")
 (description
  "A (fully) functional OCaml implementation of the simplex algorithm for solving systems of linear inequalities. The implementation is incremental and backtrackable. It is able to extract unsat-cores for unsatisfiable problems. Versions > 0.1 also support linear optimization.")
 (license "LGPL-2.1-or-later")
 (depends
  (ocaml (>= 4.02.0))
  (dune (>= 3.0))
  (ocamlfind (>= 1.9.1))
  (zarith :with-test)
  (logs (>= 0.5.0))
  (odoc :with-doc)
))
