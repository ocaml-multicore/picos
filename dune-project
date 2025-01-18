(lang dune 3.14)

(name picos)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(maintainers "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(source
 (github ocaml-multicore/picos))

(homepage https://github.com/ocaml-multicore/picos)

(version 0.6)

(license ISC)

(using mdx 0.4)

(package
 (name picos_aux)
 (synopsis "Auxiliary libraries for Picos")
 (depends
  (ocaml
   (>= 4.14.0))
  ;;
  (backoff
   (>= 0.1.1))
  (multicore-magic
   (>= 2.3.0))
  ;;
  ))

(package
 (name picos)
 (synopsis "Pico scheduler interface")
 (description
  "A systems programming interface between effects based schedulers and concurrent abstractions.")
 (depends
  (backoff
   (>= 0.1.1))
  (thread-local-storage
   (>= 0.2))
  ;;
  ))

(package
 (name picos_std)
 (synopsis "Sample libraries for Picos")
 (depends
  (picos
   (= :version))
  (picos_aux
   (= :version))
  ;;
  (backoff
   (>= 0.1.1))
  (multicore-magic
   (>= 2.3.0))
  ;;
  ))

(package
 (name picos_io)
 (synopsis "Asynchronous IO system for Picos")
 (depends
  (picos_aux
   (= :version))
  (picos_std
   (= :version))
  ;;
  (backoff
   (>= 0.1.1))
  (mtime
   (>= 2.0.0))
  (multicore-magic
   (>= 2.3.0))
  (psq
   (>= 0.2.1))
  ;;
  ))

(package
 (name picos_io_cohttp)
 (synopsis "Cohttp running on Picos IO")
 (depends
  (picos_io
   (= :version))
  ;;
  (cohttp
   (>= 6.0.0))
  (fmt
   (>= 0.9.0))
  (uri
   (>= 4.4.0))
  ;;
  ))

(package
 (name picos_mux)
 (synopsis "Sample schedulers for Picos")
 (depends
  (picos_aux
   (= :version))
  (picos_std
   (= :version))
  ;;
  (backoff
   (>= 0.1.1))
  (multicore-magic
   (>= 2.3.0))
  ;;
  )
 (depopts
  (picos_io
   (= :version))
  ;;
  ))

(package
 (name picos_lwt)
 (allow_empty)
 (synopsis "Lwt interface for Picos")
 (depends
  (picos_aux
   (= :version))
  (picos_std
   (= :version))
  ;;
  (lwt
   (>= 5.7.0))
  ;;
  ))

(package
 (name picos_meta)
 (allow_empty)
 (synopsis "Integration tests for Picos packages")
 (depends
  (picos
   (= :version))
  (picos_aux
   (= :version))
  (picos_std
   (= :version))
  (picos_io
   (= :version))
  (picos_io_cohttp
   (= :version))
  (picos_mux
   (= :version))
  (picos_lwt
   (= :version))
  ;;
  (dscheck
   (>= 0.4.0))
  (lwt
   (>= 5.7.0))
  (qcheck-core
   (>= 0.21.2))
  (qcheck-stm
   (>= 0.7))
  (qcheck-multicoretests-util
   (>= 0.7))
  ;;
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (backoff
   (and
    (>= 0.1.0)
    :with-test))
  (cohttp
   (and
    (>= 6.0.0)
    :with-test))
  (cohttp-lwt-unix
   (and
    (>= 6.0.0)
    (<> :os "win32")
    :with-test))
  (conduit-lwt-unix
   (and
    (>= 6.2.2)
    (<> :os "win32")
    :with-test))
  (conf-npm
   (and
    (<> :arch "x86_32")
    (<> :arch "riscv64")
    (<> :os "win32")
    :with-test))
  (domain_shims
   (and
    (>= 0.1.0)
    :with-test))
  (js_of_ocaml
   (and
    (>= 5.4.0)
    :with-test))
  (mdx
   (and
    (>= 2.4.0)
    :with-test))
  (multicore-bench
   (and
    (>= 0.1.7)
    :with-test))
  (multicore-magic
   (and
    (>= 2.3.0)
    :with-test))
  (multicore-magic-dscheck
   (and
    (>= 2.3.0)
    :with-test))
  (ocaml-version
   (and
    (>= 3.6.4)
    :with-test))
  (cohttp-lwt
   (and
    (>= 6.0.0)
    :with-test))
  (uri
   (and
    (>= 4.4.0)
    :with-test))
  ;;
  (odoc
   (and
    (>= 2.4.1)
    :with-doc))
  (sherlodoc
   (and
    (>= 0.2)
    :with-doc))
  ;;
  ))
