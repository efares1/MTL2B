(lang dune 3.14)

(name mtl2b)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.0)

(package
 (name mtl2b)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune menhir shexp)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

