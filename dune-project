(lang dune 3.22)

(name numscript-ml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license ISC)

(documentation https://url/to/documentation)

(using menhir 2.0)

(formatting
 (enabled_for ml))

(package
 (name numscript-ml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml menhir)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
