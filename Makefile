.PHONY: bench

bench:
	@dune exec --release -- bench/main.exe -budget 1
