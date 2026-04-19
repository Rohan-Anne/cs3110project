(lang dune 3.21)

(name ocaml-voxel)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml-voxel)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ppx_blob tsdl tgls)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
