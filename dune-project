(lang dune 3.0)
(generate_opam_files true)

(license MIT)
(authors "Imandra")
(maintainers "Imandra")
(source (github imandra-ai/batrpc))
(version 0.1)

(package
  (name batrpc)
  (synopsis "A binary RPC framework based on protobuf")
  (depends
    (ocaml (>= 4.08))
    atomic
    logs
    camlzip
    (containers (and (>= 3.0) :with-test))
    (trace :with-test)
    (trace-tef :with-test)
    (pbrt (>= 3.0))
    (pbrt_yojson (>= 3.0))
    (pbrt_services (>= 3.0)))
  (depopts
    (mtime (>= 2.0))
    base-unix))
