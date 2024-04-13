module Specs.ParserSpec where

import qualified Debug.Trace as Debug
import Test.Hspec
import YamlTransform.Parser (WhitespaceType (..), Yaml (..), parse)

test :: SpecWith ()
test = do
  describe "YamlTransform.Parser" $ do
    describe "with mappings" $ do
      context "when input contains a single mapping raw text mapping" $ do
        it "succeeds with mapping" $ do
          parse "hello: world. This is some value"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "world. This is some value"
                  ]
              ]

      context "when input contains a multiple raw text mappings" $ do
        it "succeeds with mapping" $ do
          parse "hello: world. This is some value"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "world. This is some value"
                  ]
              ]

    describe "with comments" $ do
      context "when the # doesnt have whitespace after it" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text #and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "This is some text #and then some more text"
                  ]
              ]

      context "when the # doesnt have whitespace before it" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text# and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "This is some text# and then some more text"
                  ]
              ]

      context "when a value includes a comment at the end" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text # and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "This is some text",
                    YMLWhitespace 1 WSSpace,
                    YMLComment " and then some more text"
                  ]
              ]

      context "when the comment contains # in it" $ do
        it "includes # in comment" $ do
          parse "hello: This is some text # and # then #some more# text#"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWhitespace 1 WSSpace,
                    YMLScalar "This is some text",
                    YMLWhitespace 1 WSSpace,
                    YMLComment " and # then #some more# text#"
                  ]
              ]
