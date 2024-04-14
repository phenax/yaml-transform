module Specs.ParserSpec where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Debug.Trace as Debug
import NeatInterpolation (text)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple (pShowNoColor)
import YamlTransform.Parser (Yaml (..), parse)

test :: SpecWith ()
test = do
  describe "YamlTransform.Parser" $ do
    describe "valid keys" $ do
      it "allows valid keys" $ do
        parse "-he-llo-: world"
          `shouldBe` Right
            [ YMLMapping
                "-he-llo-"
                [ YMLWSSpace,
                  YMLScalar "world"
                ]
            ]
        parse "_h_ello_: world"
          `shouldBe` Right
            [ YMLMapping
                "_h_ello_"
                [ YMLWSSpace,
                  YMLScalar "world"
                ]
            ]
        parse "$he$llo$: world"
          `shouldBe` Right
            [ YMLMapping
                "$he$llo$"
                [ YMLWSSpace,
                  YMLScalar "world"
                ]
            ]
        -- TODO: Better error messages
        parse "he#llo: world" `shouldBe` Left "Failed reading: empty"

    describe "with mappings" $ do
      context "when input contains a single mapping raw text mapping" $ do
        it "creates mapping" $ do
          parse "hello: world. This is some value"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "world. This is some value"
                  ]
              ]

      context "when input contains a multiple mappings" $ do
        it "succeeds with mapping" $ do
          let input =
                [text|
                hello: world
                foo:
                  bar: is foo
                another-item: 123
                |]
          parse input
            `shouldBe` Right
              [ YMLMapping "hello" [YMLWSSpace, YMLScalar "world"],
                YMLNewLine,
                YMLMapping
                  "foo"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "bar" [YMLWSSpace, YMLScalar "is foo"],
                    YMLNewLine
                  ],
                YMLMapping "another-item" [YMLWSSpace, YMLScalar "123"]
              ]

      context "when input contains a complex nested mappings" $ do
        it "creates nested mapping" $ do
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
                                    YMLScalar "123"
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
                                    YMLScalar "456"
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
                            YMLScalar "bar"
                          ]
                      ]
                  ]
              ]

    describe "with comments" $ do
      context "when a value includes a comment at the end of a scalar value" $ do
        it "recognizes it as a comment" $ do
          parse "hello: This is some text # and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text",
                    YMLWSSpace,
                    YMLComment " and then some more text"
                  ]
              ]

      context "when a value includes a comment at the end of mapping key" $ do
        it "recognizes it as a comment" $ do
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
                    YMLMapping "world" [YMLWSSpace, YMLScalar "123"]
                  ]
              ]

      context "when the # doesnt have whitespace after it" $ do
        it "makes it a part of the value" $ do
          parse "hello: This is some text #and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text #and then some more text"
                  ]
              ]

      context "when the # doesnt have whitespace before it" $ do
        it "makes it a part of the value" $ do
          parse "hello: This is some text# and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text# and then some more text"
                  ]
              ]

      context "when the comment contains # in it" $ do
        it "includes # in comment" $ do
          parse "hello: This is some text # and # then #some more# text#"
            `shouldBe` Right
              [ YMLMapping
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text",
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
                YMLMapping "a" [YMLWSSpace, YMLScalar "1"],
                YMLNewLine,
                YMLComment " This is another a comment",
                YMLNewLine,
                YMLMapping "b" [YMLWSSpace, YMLScalar "2"]
              ]

      context "when comments are between nested mappings" $ do
        it "creates a comment on the appropriate mapping" $ do
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
                    YMLMapping "b" [YMLWSSpace, YMLScalar "1"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLComment " This is another a comment",
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "c" [YMLWSSpace, YMLScalar "2"]
                  ]
              ]

      context "when comments are between nested mappings" $ do
        it "creates a comment on the appropriate mapping" $ do
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

    describe "with sequences" $ do
      context "when there is a sequence at root" $ do
        it "creates a sequence of scalars at root" $ do
          let input =
                [text|
            - hello
            - world
            - 123
            |]
          parse input
            `shouldBe` Right
              [ YMLSequenceItem [YMLWSSpace, YMLScalar "hello"],
                YMLNewLine,
                YMLSequenceItem [YMLWSSpace, YMLScalar "world"],
                YMLNewLine,
                YMLSequenceItem [YMLWSSpace, YMLScalar "123"]
              ]

      context "when sequence contains a mapping" $ do
        it "creates a nested mapping within the sequence" $ do
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
                    YMLMapping "hello" [YMLWSSpace, YMLScalar "world"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping "foo" [YMLWSSpace, YMLScalar "bar"],
                    YMLNewLine
                  ],
                YMLSequenceItem [YMLWSSpace, YMLScalar "123"]
              ]

      context "when mapping contains a sequence" $ do
        it "creates nested sequence + mapping" $ do
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
                    YMLSequenceItem [YMLWSSpace, YMLScalar "salad"],
                    YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem
                      [ YMLWSSpace,
                        YMLMapping "this" [YMLWSSpace, YMLScalar "that"],
                        YMLNewLine
                      ],
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLSequenceItem [YMLWSSpace, YMLScalar "fries"],
                    YMLNewLine
                  ],
                YMLMapping "foo" [YMLWSSpace, YMLScalar "bar"]
              ]

    describe "fixture tests" $ do
      let withFixture :: (Show a) => String -> (Text.Text -> IO a) -> Spec
          withFixture fixture fn = do
            let path = "fixtures/" ++ fixture
            input <- Text.pack <$> runIO (readFile $ "specs/" ++ path)
            val <- runIO $ fn input
            it path $ do
              defaultGolden path . LazyText.unpack . pShowNoColor $ val

      withFixture "basic-spaces.yml" (pure . parse)

      withFixture "basic-tabs.yml" (pure . parse)
