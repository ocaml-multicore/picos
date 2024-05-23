(lang dune 3.14)

(name picos)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(maintainers "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(source
 (github ocaml-multicore/picos))

(homepage "https://github.com/ocaml-multicore/picos")

(license "ISC")

(using mdx 0.4)

(package
 (name picos)
 (synopsis "Pico scheduler interface")
 (description
  "A systems programming interface between effects based schedulers and concurrent abstractions.")
 (depends
  (backoff
   (>= 0.1.0))
  ;; For OCaml 4 compatible defaults that are not used on OCaml 5 by default
  (thread-local-storage
   (>= 0.1))
  (mtime
   (>= 2.0.0))
  (psq
   (>= 0.2.1))
  ;; For mpsc queue
  (multicore-magic
   (>= 2.2.0))
  ;; For picos.lwt
  (lwt
   (>= 5.7.0))
  ;; Test dependencies
  (multicore-bench
   (and
    (>= 0.1.3)
    :with-test))
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (qcheck-core
   (and
    (>= 0.21.2)
    :with-test))
  (qcheck-stm
   (and
    (>= 0.3)
    :with-test))
  (qcheck-multicoretests-util
   (and
    (>= 0.3)
    :with-test))
  (mdx
   (and
    (>= 2.4.0)
    :with-test))
  (ocaml-version
   (and
    (>= 3.6.4)
    :with-test))
  (domain_shims
   (and
    (>= 0.1.0)
    :with-test))
  (js_of_ocaml
   (and
    (>= 5.4.0)
    :with-test))
  (conf-npm
   (and
    (<> :arch "x86_32")
    (<> :arch "riscv64")
    :with-test))
  (dscheck
   (and
    (>= 0.4.0)
    :with-test))
  ;; Documentation dependencies
  (sherlodoc
   (and
    (>= 0.2)
    :with-doc))
  (odoc
   (and
    (>= 2.4.1)
    :with-doc))
  (ocaml
   (>= 4.14.0))))
