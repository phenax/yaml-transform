module Specs.ParserSpec where

import Data.Either (isLeft)
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
import YamlTransform.Types (YMLScalar (ScalarDoubleQuote, ScalarNumber, ScalarRawString, ScalarSingleQuote))

test :: SpecWith ()
test = do
  describe "YamlTransform.Parser" $ do
    describe "keys" $ do
      it "allows valid keys" $ do
        parse "-he-llo-: world"
          `shouldBe` Right
            [ YMLMapping
                "-he-llo-"
                [ YMLWSSpace,
                  YMLScalar $ ScalarRawString "world"
                ]
            ]
        parse "_h_ello_: world"
          `shouldBe` Right
            [ YMLMapping
                "_h_ello_"
                [ YMLWSSpace,
                  YMLScalar $ ScalarRawString "world"
                ]
            ]
        parse "$he$llo$: world"
          `shouldBe` Right
            [ YMLMapping
                "$he$llo$"
                [ YMLWSSpace,
                  YMLScalar $ ScalarRawString "world"
                ]
            ]
      it "does not allow invalid keys" $ do
        parse "he#llo: world" `shouldBe` Left "Failed reading: empty"

    describe "with scalars" $ do
      context "when given a raw string" $ do
        it "parses mapping" $ do
          parse "hello: world !@#$%^&()_+-=;':,./<>?"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "world !@#$%^&()_+-=;':,./<>?"
                  ]
              ]

      context "when invalid characters for raw string" $ do
        it "parses mapping" $ do
          parse "hello: jello [" `shouldSatisfy` isLeft
          parse "hello: jello ]" `shouldSatisfy` isLeft
          parse "hello: jello {" `shouldSatisfy` isLeft
          parse "hello: jello }" `shouldSatisfy` isLeft

      context "when given a delimited string" $ do
        it "parses mapping" $ do
          parse "hello: 'world'"
            `shouldBe` Right
              [YMLMapping "hello" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "world"]]

        it "parses mapping" $ do
          parse "hello: \"world\""
            `shouldBe` Right
              [YMLMapping "hello" [YMLWSSpace, YMLScalar $ ScalarDoubleQuote "world"]]

      context "when given a delimited string crossing multiple lines" $ do
        it "parses mapping" $ do
          let input =
                [text|
            key: "hello world
              testing mutliline
              here"
            |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "key"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarDoubleQuote "hello world\n  testing mutliline\n  here"
                  ]
              ]

    describe "with mappings" $ do
      context "when input contains a single mapping raw text mapping" $ do
        it "parses mapping" $ do
          parse "hello: world. This is some value"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "world. This is some value"
                  ]
              ]

      context "when input contains a multiple mappings" $ do
        it "parses mappings" $ do
          let input =
                [text|
                hello: world
                foo:
                  bar: is foo
                another-item: 123
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping "hello" [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                YMLNewLine,
                YMLMapping
                  "foo"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "bar" [YMLWSSpace, YMLScalar $ ScalarRawString "is foo"],
                    YMLNewLine
                  ],
                YMLMapping "another-item" [YMLWSSpace, YMLScalar $ ScalarNumber 123]
              ]

      context "when input contains a complex nested mappings" $ do
        it "parses nested mapping" $ do
          let input =
                [text|
                root:
                  a:
                    b:
                      c:
                        d: 123
                        e: 456
                    foo: bar
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "root"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      "a"
                      [ YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping
                          "b"
                          [ YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLMapping
                              "c"
                              [ YMLNewLine,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLMapping
                                  "d"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarNumber 123
                                  ],
                                YMLNewLine,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLWSSpace,
                                YMLMapping
                                  "e"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarNumber 456
                                  ],
                                YMLNewLine
                              ]
                          ],
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping
                          "foo"
                          [ YMLWSSpace,
                            YMLScalar $ ScalarRawString "bar"
                          ]
                      ]
                  ]
              ]

    describe "with comments" $ do
      context "when a value includes a comment at the end of a scalar value" $ do
        it "parses it as a comment" $ do
          parse "hello: This is some text # and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text",
                    YMLWSSpace,
                    YMLComment " and then some more text"
                  ]
              ]

      context "when a value includes a comment at the end of mapping key" $ do
        it "parses it as a comment" $ do
          let input =
                [text|
                hello: # This is a comment
                  world: 123
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLComment " This is a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "world" [YMLWSSpace, YMLScalar $ ScalarNumber 123.0]
                  ]
              ]

      context "when the # doesnt have whitespace after it" $ do
        it "parses it as a comment" $ do
          parse "hello: This is some text #and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text",
                    YMLWSSpace,
                    YMLComment "and then some more text"
                  ]
              ]

      context "when the # doesnt have whitespace before it" $ do
        it "makes it a part of the value" $ do
          parse "hello: This is some text# and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text# and then some more text"
                  ]
              ]

      context "when the comment contains # in it" $ do
        it "includes # in comment" $ do
          parse "hello: This is some text # and # then #some more# text#"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text",
                    YMLWSSpace,
                    YMLComment " and # then #some more# text#"
                  ]
              ]

      context "when comments are above mappings" $ do
        it "includes # in comment" $ do
          let input =
                [text|
                # This is a comment
                a: 1
                # This is another a comment
                b: 2
                |]
          parse input
            `shouldBe` Right
              [ YMLComment " This is a comment",
                YMLNewLine,
                YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarNumber 1],
                YMLNewLine,
                YMLComment " This is another a comment",
                YMLNewLine,
                YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarNumber 2]
              ]

      context "when comments are between nested mappings" $ do
        it "parses a comment on the appropriate mapping" $ do
          let input =
                [text|
                a:
                  b: 1
                  # This is another a comment
                  c: 2
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "a"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarNumber 1],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarNumber 2]
                  ]
              ]

      context "when comments are between nested mappings" $ do
        it "parses a comment on the appropriate mapping" $ do
          let input =
                [text|
                a:
                  b:
                    c: 2
                  # This is another a comment
                  d: 5
                c: 2
                |]
          parse input
            `shouldBe` Right
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
                        YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarNumber 2],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "d" [YMLWSSpace, YMLScalar $ ScalarNumber 5],
                    YMLNewLine
                  ],
                YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarNumber 2]
              ]

    describe "with sequences" $ do
      context "when there is a sequence at root" $ do
        it "parses a sequence of scalars at root" $ do
          let input =
                [text|
            - hello
            - world
            - 123
            |]
          parse input
            `shouldBe` Right
              [ YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarRawString "hello"],
                YMLNewLine,
                YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                YMLNewLine,
                YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarNumber 123]
              ]

      context "when sequence contains a mapping" $ do
        it "parses a nested mapping within the sequence" $ do
          let input =
                [text|
            - hello: world
              foo: bar
            - 123
            |]
          parse input
            `shouldBe` Right
              [ YMLSequenceItem
                  [ YMLWSSpace,
                    YMLMapping "hello" [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"],
                    YMLNewLine
                  ],
                YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarNumber 123]
              ]

      context "when mapping contains a sequence" $ do
        it "parses nested sequence + mapping" $ do
          let input =
                [text|
                hello:
                  - salad
                  - this: that
                  - fries
                foo: bar
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarRawString "salad"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      [ YMLWSSpace,
                        YMLMapping "this" [YMLWSSpace, YMLScalar $ ScalarRawString "that"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarRawString "fries"],
                    YMLNewLine
                  ],
                YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
              ]

      context "when mapping starts on the line below the start of a sequence" $ do
        it "parses a the mapping inside sequence item" $ do
          let input =
                [text|
                - hello
                -
                  key1: v1
                  key2: v2
                - 123
                |]
          parse input
            `shouldBe` Right
              [ YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarRawString "hello"],
                YMLNewLine,
                YMLSequenceItem
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "key1" [YMLWSSpace, YMLScalar $ ScalarRawString "v1"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "key2" [YMLWSSpace, YMLScalar $ ScalarRawString "v2"],
                    YMLNewLine
                  ],
                YMLSequenceItem [YMLWSSpace, YMLScalar $ ScalarNumber 123]
              ]

    describe "with inline sequence" $ do
      context "when there is an inline sequence as a mapping value" $ do
        it "parses the inline sequence of scalars" $ do
          parse "mapping: ['hello', 'wo,rld', 123]"
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarSingleQuote "hello"],
                        [YMLWSSpace, YMLScalar $ ScalarSingleQuote "wo,rld"],
                        [YMLWSSpace, YMLScalar $ ScalarNumber 123]
                      ]
                  ]
              ]

      context "when there is an inline sequence as a sequence value" $ do
        it "parses the inline sequence of scalars" $ do
          parse "- ['hello', 'wo,rld', 123]"
            `shouldBe` Right
              [ YMLSequenceItem
                  [ YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarSingleQuote "hello"],
                        [YMLWSSpace, YMLScalar $ ScalarSingleQuote "wo,rld"],
                        [YMLWSSpace, YMLScalar $ ScalarNumber 123]
                      ]
                  ]
              ]

      context "when the inline sequence is split into multiple lines" $ do
        it "parses the inline sequence of scalars" $ do
          let input =
                [text|
              root:
                mapping: [ 'hello',
                    'wo,rld',
                123
              ]
            |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "root"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      "mapping"
                      [ YMLWSSpace,
                        YMLInlineSequence
                          [ [YMLWSSpace, YMLScalar $ ScalarSingleQuote "hello"],
                            [ YMLNewLine,
                              YMLWSSpace,
                              YMLWSSpace,
                              YMLWSSpace,
                              YMLWSSpace,
                              YMLWSSpace,
                              YMLWSSpace,
                              YMLScalar $ ScalarSingleQuote "wo,rld"
                            ],
                            [YMLNewLine, YMLWSSpace, YMLWSSpace, YMLScalar $ ScalarNumber 123, YMLNewLine]
                          ]
                      ]
                  ]
              ]

      context "when the inline sequence is split into multiple lines with comments" $ do
        it "parses the inline sequence of scalars" $ do
          let input =
                [text|
                mapping: [
                  'hello', # Comment 1
                  123 # Comment 2
                ] # Comment 3
            |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLWSSpace,
                    YMLInlineSequence
                      [ [ YMLNewLine,
                          YMLWSSpace,
                          YMLWSSpace,
                          YMLScalar $ ScalarSingleQuote "hello"
                        ],
                        [ YMLWSSpace,
                          YMLComment " Comment 1",
                          YMLNewLine,
                          YMLWSSpace,
                          YMLWSSpace,
                          YMLScalar $ ScalarNumber 123,
                          YMLWSSpace,
                          YMLComment " Comment 2",
                          YMLNewLine
                        ]
                      ],
                    YMLWSSpace,
                    YMLComment " Comment 3"
                  ]
              ]

    describe "with anchors" $ do
      context "when there is an achor on a mapping with scalar" $ do
        it "parses anchor" $ do
          parse "mapping: &my-anchor This is some text that follows"
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text that follows"
                  ]
              ]

      context "when there is an achor on a nested mapping" $ do
        it "parses anchor" $ do
          let input =
                [text|
            mapping: &my-anchor
              foo: bar
              items: wow
          |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "items" [YMLWSSpace, YMLScalar $ ScalarRawString "wow"]
                  ]
              ]

      context "when there is an achor on a sequence" $ do
        it "parses anchor" $ do
          parse "- &my-anchor This is some text that follows"
            `shouldBe` Right
              [ YMLSequenceItem
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLWSSpace,
                    YMLScalar $ ScalarRawString "This is some text that follows"
                  ]
              ]

      context "when there is an achor on a sequence inside a mapping" $ do
        it "parses anchor" $ do
          let input =
                [text|
            mapping:
              - &anchor-1 123
              - &anchor-2
                key: "value 1"
              - world
          |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      [ YMLWSSpace,
                        YMLAnchor "anchor-1",
                        YMLWSSpace,
                        YMLScalar $ ScalarNumber 123
                      ],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      [ YMLWSSpace,
                        YMLAnchor "anchor-2",
                        YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping "key" [YMLWSSpace, YMLScalar $ ScalarDoubleQuote "value 1"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      [ YMLWSSpace,
                        YMLScalar $ ScalarRawString "world"
                      ]
                  ]
              ]

      context "when there is an achor on an inline sequence" $ do
        it "parses anchor" $ do
          parse "mapping: &my-anchor [1, 'two']"
            `shouldBe` Right
              [ YMLMapping
                  "mapping"
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarNumber 1],
                        [YMLWSSpace, YMLScalar $ ScalarSingleQuote "two"]
                      ]
                  ]
              ]

    describe "fixture tests" $ do
      let toGolden = LazyText.unpack . pShowNoColor

      describe "basic.yml" $ do
        it "parses correctly" $ do
          let input = decodeLatin1 $(embedFile "specs/fixtures/basic.yml")
          defaultGolden "parser-fixtures/basic.yml" . toGolden . parse $ input
