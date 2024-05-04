default:
  @just --choose

run *args:
  cabal run yaml-transform -- {{args}}

test:
  cabal test

testw:
  npx nodemon -e .hs -w src --exec 'ghcid -c "cabal repl test:specs" -T :main'

testg:
  hgold -u .golden
  @just test

build:
  nix build

