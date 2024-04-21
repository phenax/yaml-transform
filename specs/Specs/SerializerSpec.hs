module Specs.SerializerSpec where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Debug.Trace as Debug
import NeatInterpolation (text)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple (pShowNoColor)
import YamlTransform.Parser (Yaml (..), parse)
import YamlTransform.Serializer (serialize)

test :: SpecWith ()
test = do
  describe "YamlTransform.Serialer" $ do
    describe "nested mappings with comments" $ do
      it "allows valid keys" $ do
        let input =
              [ YMLMapping
                  "a"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      "b"
                      [ YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping "c" [YMLWSSpace, YMLScalar "2"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "d" [YMLWSSpace, YMLScalar "5"],
                    YMLNewLine
                  ],
                YMLMapping "c" [YMLWSSpace, YMLScalar "2"]
              ]
        serialize input
          `shouldBe` [text|
                a:
                  b:
                    c: 2
                  # This is another a comment
                  d: 5
                c: 2
          |]

    describe "with inline mapping" $ do
      context "when inline mapping with comments is present" $ do
        it "serializes it correctly" $ do
          let input =
                [ YMLMapping
                    "mapping"
                    [ YMLWSSpace,
                      YMLInlineSequence
                        [ [ YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLScalar "hello"
                          ],
                          [ YMLWSSpace,
                            YMLComment " Comment 1",
                            YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLScalar "123.0",
                            YMLWSSpace,
                            YMLComment " Comment 2",
                            YMLNewLine
                          ]
                        ],
                      YMLWSSpace,
                      YMLComment " Comment 3"
                    ]
                ]
          serialize input
            `shouldBe` [text|
              mapping: [
                hello, # Comment 1
                123.0 # Comment 2
              ] # Comment 3
            |]

    describe "fixture tests" $ do
      let withFixture fixture fn = do
            let path = "fixtures/" ++ fixture
            describe path $ do
              context "parsed fixture" $ do
                input <- Text.pack <$> runIO (readFile $ "specs/" ++ path)
                fn input $ parse input

      withFixture "basic.yml" $ \input parsed -> do
        it "serializes to original file" $ do
          fmap serialize parsed `shouldBe` Right input
