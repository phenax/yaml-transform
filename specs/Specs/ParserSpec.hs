module Specs.ParserSpec where

import Test.Hspec

test :: SpecWith ()
test = do
  describe "parse literals" $ do
    it "literals" $ do
      1 `shouldBe` 1
