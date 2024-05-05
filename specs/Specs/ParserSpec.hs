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
import YamlTransform.Types (YMLScalar (ScalarDoubleQuote, ScalarRawString, ScalarSingleQuote))

test :: SpecWith ()
test = do
  describe "YamlTransform.Parser" $ do
    describe "keys" $ do
      it "allows valid keys" $ do
        parse "-he-llo-: world"
          `shouldBe` Right
            [ YMLMapping
                0
                "-he-llo-"
                [ YMLWSSpace,
                  YMLScalar $ ScalarRawString "world"
                ]
            ]
        parse "_h_ello_: world"
          `shouldBe` Right
            [ YMLMapping
                0
                "_h_ello_"
                [ YMLWSSpace,
                  YMLScalar $ ScalarRawString "world"
                ]
            ]
        parse "$he$llo$: world"
          `shouldBe` Right
            [ YMLMapping
                0
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
                  0
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
              [YMLMapping 0 "hello" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "world"]]

        it "parses mapping" $ do
          parse "hello: \"world\""
            `shouldBe` Right
              [YMLMapping 0 "hello" [YMLWSSpace, YMLScalar $ ScalarDoubleQuote "world"]]

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
                  0
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
                  0
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
              [ YMLMapping 0 "hello" [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                YMLNewLine,
                YMLMapping
                  0
                  "foo"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "bar" [YMLWSSpace, YMLScalar $ ScalarRawString "is foo"],
                    YMLNewLine
                  ],
                YMLMapping 0 "another-item" [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
              ]

      context "when input contains a complex nested mappings" $ do
        it "parses nested mapping" $ do
          let input =
                [text|
                root:
                  a:
                    b:
                      c:
                        d: 123.0
                        e: 456
                    foo: bar
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping
                  0
                  "root"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      2
                      "a"
                      [ YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping
                          4
                          "b"
                          [ YMLNewLine,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLWSSpace,
                            YMLMapping
                              6
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
                                  8
                                  "d"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarRawString "123.0"
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
                                  8
                                  "e"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarRawString "456"
                                  ],
                                YMLNewLine
                              ]
                          ],
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping
                          4
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
                  0
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
                  0
                  "hello"
                  [ YMLWSSpace,
                    YMLComment " This is a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "world" [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
                  ]
              ]

      context "when the # doesnt have whitespace after it" $ do
        it "parses it as a comment" $ do
          parse "hello: This is some text #and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  0
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
                  0
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
                  0
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
                YMLMapping 0 "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                YMLNewLine,
                YMLComment " This is another a comment",
                YMLNewLine,
                YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"]
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
                  0
                  "a"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "2"]
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
                  0
                  "a"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      2
                      "b"
                      [ YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping 4 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "5"],
                    YMLNewLine
                  ],
                YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "2"]
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
              [ YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "hello"],
                YMLNewLine,
                YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                YMLNewLine,
                YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
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
                  0
                  [ YMLWSSpace,
                    YMLMapping 1 "hello" [YMLWSSpace, YMLScalar $ ScalarRawString "world"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"],
                    YMLNewLine
                  ],
                YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
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
                  0
                  "hello"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem 2 [YMLWSSpace, YMLScalar $ ScalarRawString "salad"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      2
                      [ YMLWSSpace,
                        YMLMapping 3 "this" [YMLWSSpace, YMLScalar $ ScalarRawString "that"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem 2 [YMLWSSpace, YMLScalar $ ScalarRawString "fries"],
                    YMLNewLine
                  ],
                YMLMapping 0 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
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
              [ YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "hello"],
                YMLNewLine,
                YMLSequenceItem
                  0
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "key1" [YMLWSSpace, YMLScalar $ ScalarRawString "v1"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "key2" [YMLWSSpace, YMLScalar $ ScalarRawString "v2"],
                    YMLNewLine
                  ],
                YMLSequenceItem 0 [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
              ]

    describe "with inline sequence" $ do
      context "when there is an inline sequence as a mapping value" $ do
        it "parses the inline sequence of scalars" $ do
          parse "mapping: ['hello', 'wo,rld', 123]"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "mapping"
                  [ YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarSingleQuote "hello"],
                        [YMLWSSpace, YMLScalar $ ScalarSingleQuote "wo,rld"],
                        [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
                      ]
                  ]
              ]

      context "when there is an inline sequence as a sequence value" $ do
        it "parses the inline sequence of scalars" $ do
          parse "- ['hello', 'wo,rld', 123]"
            `shouldBe` Right
              [ YMLSequenceItem
                  0
                  [ YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarSingleQuote "hello"],
                        [YMLWSSpace, YMLScalar $ ScalarSingleQuote "wo,rld"],
                        [YMLWSSpace, YMLScalar $ ScalarRawString "123"]
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
                  0
                  "root"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping
                      2
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
                            [YMLNewLine, YMLWSSpace, YMLWSSpace, YMLScalar $ ScalarRawString "123", YMLNewLine]
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
                  0
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
                          YMLScalar $ ScalarRawString "123",
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
                  0
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
                  0
                  "mapping"
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "items" [YMLWSSpace, YMLScalar $ ScalarRawString "wow"]
                  ]
              ]

      context "when there is an achor on a sequence" $ do
        it "parses anchor" $ do
          parse "- &my-anchor This is some text that follows"
            `shouldBe` Right
              [ YMLSequenceItem
                  0
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
                  0
                  "mapping"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      2
                      [ YMLWSSpace,
                        YMLAnchor "anchor-1",
                        YMLWSSpace,
                        YMLScalar $ ScalarRawString "123"
                      ],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      2
                      [ YMLWSSpace,
                        YMLAnchor "anchor-2",
                        YMLNewLine,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLWSSpace,
                        YMLMapping 6 "key" [YMLWSSpace, YMLScalar $ ScalarDoubleQuote "value 1"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      2
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
                  0
                  "mapping"
                  [ YMLWSSpace,
                    YMLAnchor "my-anchor",
                    YMLWSSpace,
                    YMLInlineSequence
                      [ [YMLScalar $ ScalarRawString "1"],
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

      describe "with-anchors.yml" $ do
        it "parses correctly" $ do
          let input = decodeLatin1 $(embedFile "specs/fixtures/with-anchors.yml")
          defaultGolden "parser-fixtures/with-anchors.yml" . toGolden . parse $ input
