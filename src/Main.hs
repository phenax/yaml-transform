module Main where

import YamlTransform.Parser (parse)

main :: IO ()
main = print $ parse "Hello, Haskell!"
