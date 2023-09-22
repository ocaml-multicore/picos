(lang dune 3.8)

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
  (backoff
   (>= 0.1.0))
  (mtime
   (>= 2.0.0))
  (psq
   (>= 0.2.1))
  (alcotest
   (and
    (>= 1.7.0)
    :with-test))
  (mdx
   (and
    (>= 2.3.1)
    :with-test))
  (ocaml
   (>= 5.0.0))))
