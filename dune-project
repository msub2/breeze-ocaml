(lang dune 3.16)

(name breeze)

(generate_opam_files true)

(source
 (github msub2/breeze))

(authors "Daniel Adams")

(maintainers "Daniel Adams")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name breeze)
 (synopsis "A SmolNet browser")
 (description "A browser focused on accessing content from smaller protocols like Gopher, Gemini, etc.")
 (depends ocaml dune)
 (tags
  (browser gopher gemini spartan guppy nex)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
