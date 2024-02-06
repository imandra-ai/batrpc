(lang dune 3.0)
(generate_opam_files true)

(license MIT)
(authors "Imandra")
(maintainers "Imandra")
(source (github imandra-ai/ezrpc))
(version 0.1)

(package
  (name ezrpc)
  (depends
    (ocaml (>= 4.08))
    atomic
    logs
    (pbrt (>= 3.0))
    (pbrt_yojson (>= 3.0))
    (pbrt_services (>= 3.0)))
  (depopts
    base-unix))
