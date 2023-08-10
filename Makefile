all: build
build:
	@dune build src
clean:
	@dune clean
dev:
	@dune build
