(lang dune 3.0)
(generate_opam_files true)

(name batrpc)
(license MIT)
(authors "Imandra")
(maintainers "Imandra")
(source (github imandra-ai/batrpc))
(version 0.1)

(package
  (name batrpc)
  (synopsis "A binary RPC framework based on protobuf")
  (depends
    (ocaml (>= 5.0))
    atomic
    logs
    camlzip
    imandrakit
    imandrakit-log
    imandrakit-thread
    ppx_deriving
    ;(ocaml-protoc (and (>= 3.0) (< 4.0)))
    (iostream (>= 0.2.2))
    hmap
    (pbrt (>= 3.0))
    (pbrt_yojson (>= 3.0))
    (pbrt_services (>= 3.0))
    (moonpool (>= 0.5))
    (containers (and (>= 3.0) :with-test))
    (trace-tef :with-test)
    (mtime (>= 2.0))
    (trace (>= 0.6))))
    

(package
  (name batrpc-otel)
  (synopsis "opentelemetry middleware for BatRPC")
  (depends
    (batrpc (= :version))
    (opentelemetry (>= 0.8))
    ))
