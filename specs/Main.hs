module Main (main) where

import qualified Specs.ParserSpec
import qualified Specs.SerializerSpec
import qualified Specs.TransformerSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.ParserSpec.test
  Specs.SerializerSpec.test
  Specs.TransformerSpec.test
