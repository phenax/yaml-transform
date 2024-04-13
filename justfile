default:
    @just --list

test:
  cabal test

testw:
  ghcid --reload=src --reload=specs -c "cabal repl test:specs" -T :main

build:
  nix build
