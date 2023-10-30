(lang dune 3.9)

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
 (synopsis "Pico scheduler framework")
 (depends
  (thread-local-storage
   (>= 0.1))
  (backoff
   (>= 0.1.0))
  (multicore-magic
   (>= 2.0.0))
  (mtime
   (>= 2.0.0))
  (psq
   (>= 0.2.1))
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
  (mdx
   (and
    (>= 2.3.1)
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
    :with-test))
  (saturn
   (and
    (>= 0.4.1)
    :with-test))
  (saturn_lockfree
   (and
    (>= 0.4.1)
    :with-test))
  (ocaml
   (>= 4.12.0))))
