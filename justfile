default:
    @just --list

test:
  cabal test

testw:
  npx nodemon -e .hs -w src --exec 'ghcid -c "cabal repl test:specs" -T :main'

build:
  nix build
