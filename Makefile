.PHONY: bench

bench:
	@dune exec --release -- bench/main.exe -budget 1

watch:
	@dune build --release @check -w
