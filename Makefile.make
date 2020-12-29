.PHONY: build

build:
  cabal install
  cabal sdist
