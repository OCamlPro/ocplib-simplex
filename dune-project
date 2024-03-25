(lang dune 2.0)

(name ocplib-simplex)

(version 0.5)

(generate_opam_files true)

(source
 (github OCamlPro/ocplib-simplex))

(authors
  "Mohamed Iguernlala <iguer@functori.com>"
  "Hichem Ait El Hara <hichem.ait-el-hara@ocamlpro.com>"
  "Steven de Oliveira <steven.de-oliveira@ocamlpro.com>"
  "Guillaume Bury <guillaume.bury@ocamlpro.com>"
)

(maintainers "OCamlPro <contact@ocamlpro.com>")

(license LICENSE)

(documentation https://github.com/OCamlPro/ocplib-simplex)

(package
 (name ocplib-simplex)
 (synopsis
  "ocplib-simplex: A library implementing a simplex algorithm, in a functional style, for solving systems of linear inequalities")
 (description
  "ocplib-simplex is a (fully) functional OCaml implementation of the simplex algorithm for solving systems of linear inequalities. The implementation is incremental and backtrackable. It is able to extract unsat-cores for unsatisfiable problems. Versions > 0.1 also support linear optimization.")
 (depends
  (ocaml (>= 4.02.0))
  (dune (>= 2.0))
  (ocamlfind (>= 1.9.1))
  (zarith :with-test)
  (logs (>= 0.5.0))
  (odoc :with-doc)
))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
