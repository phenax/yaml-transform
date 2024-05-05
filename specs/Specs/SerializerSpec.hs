module Specs.SerializerSpec where

import Data.FileEmbed (embedFile)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.Lazy as LazyText
import qualified Debug.Trace as Debug
import NeatInterpolation (text)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple (pShowNoColor)
import YamlTransform.Parser (Yaml (..), parse)
import YamlTransform.Serializer (serialize)
import YamlTransform.Types (YMLScalar (..))

test :: SpecWith ()
test = do
  describe "YamlTransform.Serialer" $ do
    describe "nested mappings with comments" $ do
      it "allows valid keys" $ do
        let input =
              [ YMLMapping
                  0
                  "a"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      0
                      "b"
                      [ YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 0 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "5"],
                    YMLNewLine
                  ],
                YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "2"]
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
                    0
                    "mapping"
                    [ YMLWSSpace,
                      YMLInlineSequence
                        [ [ YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLScalar $ ScalarRawString "hello"
                          ],
                          [ YMLWSSpace,
                            YMLComment " Comment 1",
                            YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLScalar $ ScalarRawString "123.0",
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

    describe "with anchors" $ do
      context "when mapping has anchor" $ do
        it "serializes it correctly" $ do
          let input =
                [ YMLMapping
                    0
                    "mapping"
                    [ YMLWSSpace,
                      YMLAnchor "some-anchor",
                      YMLWSSpace,
                      YMLScalar $ ScalarRawString "This text"
                    ]
                ]
          serialize input
            `shouldBe` [text|
              mapping: &some-anchor This text
            |]

      context "when sequence has anchor" $ do
        it "serializes it correctly" $ do
          let input =
                [ YMLMapping
                    0
                    "mapping"
                    [ YMLNewLine,
                      YMLWSSpace,
                      YMLWSSpace,
                      YMLSequenceItem
                        0
                        [ YMLWSSpace,
                          YMLAnchor "some-anchor",
                          YMLNewLine,
                          YMLWSSpace,
                          YMLWSSpace,
                          YMLWSSpace,
                          YMLWSSpace,
                          YMLMapping 0 "one" [YMLWSSpace, YMLScalar $ ScalarRawString "two"]
                        ],
                      YMLNewLine,
                      YMLWSSpace,
                      YMLWSSpace,
                      YMLSequenceItem 0 [YMLWSSpace, YMLAnchor "another", YMLWSSpace, YMLScalar $ ScalarRawString "More"]
                    ]
                ]
          serialize input
            `shouldBe` [text|
            mapping:
              - &some-anchor
                one: two
              - &another More
            |]

    describe "fixture tests" $ do
      it "basic.yml serializes to original file contents" $ do
        let input = decodeLatin1 $(embedFile "specs/fixtures/basic.yml")
        fmap serialize (parse input) `shouldBe` Right input

      it "with-anchors.yml serializes to original file contents" $ do
        let input = decodeLatin1 $(embedFile "specs/fixtures/with-anchors.yml")
        fmap serialize (parse input) `shouldBe` Right input
