# Guide to contributing to Ocplib-simplex

## Release Process

- Make a PR on `master` to initiate the release. The PR must introduce
  a new section in `CHANGES.md` with the new version number;
- Once the PR is merged, create a new `vX.Y.Z` branch for the release from the
  current `master`.

## Make the release

The release is made using `dune-release`, and follows standard procedure for
the tool:
- `dune-release check` performs basic check;
- `dune-release tag vX.Y.Z` creates a tag on the current branch;
- `dune-release distrib` creates the distribution archive;
- `dune-release publish` publishes the release on GitHub;
- `dune-release opam pkg` creates an archive for opam;
- `dune-release opam submit` opens a PR on the opam repository.
