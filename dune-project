(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/protype-robin))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name protype-robin)
 (synopsis "Robin encodings for types described using Protype")
 (description "Encode and decode values whose type has been described using Protype, in the Robin binary format.")
 (depends
  (ocaml (>= 4.07))
  protype
  robin))
