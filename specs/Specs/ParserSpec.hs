module Specs.ParserSpec where

import qualified Debug.Trace as Debug
import NeatInterpolation (text)
import Test.Hspec
import YamlTransform.Parser (WhitespaceType (..), Yaml (..), parse)

test :: SpecWith ()
test = do
  describe "YamlTransform.Parser" $ do
    describe "with mappings" $ do
      context "when input contains a single mapping raw text mapping" $ do
        it "creates mapping" $ do
          parse "hello: world. This is some value"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "world. This is some value"
                  ]
              ]

      describe "valid keys" $ do
        it "creates mapping" $ do
          parse "-he-llo-: world"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "-he-llo-"
                  [ YMLWSSpace,
                    YMLScalar "world"
                  ]
              ]
          parse "_h_ello_: world"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "_h_ello_"
                  [ YMLWSSpace,
                    YMLScalar "world"
                  ]
              ]
          parse "$he$llo$: world"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "$he$llo$"
                  [ YMLWSSpace,
                    YMLScalar "world"
                  ]
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
                                  8
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
                          4
                          "foo"
                          [ YMLWSSpace,
                            YMLScalar "bar"
                          ]
                      ]
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
          Debug.traceM $ show input
          parse input
            `shouldBe` Right
              [ YMLMapping 0 "hello" [YMLWSSpace, YMLScalar "world"],
                YMLNewLine,
                YMLMapping
                  0
                  "foo"
                  [ YMLNewLine,
                    YMLWSSpace,
                    YMLWSSpace,
                    YMLMapping 2 "bar" [YMLWSSpace, YMLScalar "is foo"],
                    YMLNewLine
                  ],
                YMLMapping 0 "another-item" [YMLWSSpace, YMLScalar "123"]
              ]

    describe "with comments" $ do
      context "when the # doesnt have whitespace after it" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text #and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text #and then some more text"
                  ]
              ]

      context "when the # doesnt have whitespace before it" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text# and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text# and then some more text"
                  ]
              ]

      context "when a value includes a comment at the end" $ do
        it "succeeds with mapping" $ do
          parse "hello: This is some text # and then some more text"
            `shouldBe` Right
              [ YMLMapping
                  0
                  "hello"
                  [ YMLWSSpace,
                    YMLScalar "This is some text",
                    YMLWSSpace,
                    YMLComment " and then some more text"
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
                    YMLScalar "This is some text",
                    YMLWSSpace,
                    YMLComment " and # then #some more# text#"
                  ]
              ]
