module Specs.TransformerSpec where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Debug.Trace as Debug
import NeatInterpolation (text)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple (pShowNoColor)
import YamlTransform.Parser (Yaml (..), parse)
import YamlTransform.Serializer (serialize)
import YamlTransform.Transformer (updateKey, updatePath, updateScalarAtPath)
import YamlTransform.Types (YMLScalar (ScalarRawString, ScalarSingleQuote))

test :: SpecWith ()
test = do
  describe "YamlTransform.Transformer" $ do
    describe "updateKey" $ do
      let doc =
            [ YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
              YMLNewLine,
              YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
              YMLNewLine,
              YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
            ]

      context "when yaml doc contains the key" $ do
        it "updates the value of the key" $ do
          let (isUpdated, newDoc) = updateKey "b" (const [YMLWSSpace, YMLScalar $ ScalarRawString "99"]) doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                         YMLNewLine,
                         YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "99"],
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]

      context "when yaml doc does not contains the key" $ do
        it "inserts new mapping" $ do
          let (isUpdated, newDoc) = updateKey "z" (const [YMLWSSpace, YMLScalar $ ScalarRawString "99"]) doc
          isUpdated `shouldBe` False
          newDoc
            `shouldBe` [ YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                         YMLNewLine,
                         YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"],
                         YMLNewLine,
                         YMLMapping "z" [YMLWSSpace, YMLScalar $ ScalarRawString "99"],
                         YMLNewLine
                       ]

    describe "updatePath" $ do
      let doc =
            [ YMLMapping
                "root"
                [ YMLNewLine,
                  YMLMapping
                    "a"
                    [ YMLNewLine,
                      YMLMapping
                        "b"
                        [ YMLNewLine,
                          YMLMapping
                            "c"
                            [ YMLNewLine,
                              YMLMapping "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                              YMLNewLine,
                              YMLMapping "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                              YMLNewLine,
                              YMLMapping "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                              YMLNewLine
                            ]
                        ],
                      YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                    ]
                ]
            ]

      context "when yaml doc contains the path" $ do
        it "updates the value at the path" $ do
          let updater = const [YMLWSSpace, YMLScalar $ ScalarRawString "999"]
          let (isUpdated, newDoc) = updatePath ["root", "a", "b", "c", "e"] updater doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping "e" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine,
                                         YMLMapping "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                               ]
                           ]
                       ]

      context "when yaml doc does not contains the path" $ do
        it "inserts new mapping" $ do
          let updater = const [YMLWSSpace, YMLScalar $ ScalarRawString "999"]
          let (isUpdated, newDoc) = updatePath ["root", "a", "b", "c", "coocoo"] updater doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                                         YMLNewLine,
                                         YMLMapping "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine,
                                         YMLMapping "coocoo" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                               ]
                           ]
                       ]

        it "inserts new mapping" $ do
          let updater = const [YMLWSSpace, YMLScalar $ ScalarRawString "999"]
          let (isUpdated, newDoc) = updatePath ["root", "a", "b", "coocoo", "e"] updater doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                                         YMLNewLine,
                                         YMLMapping "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine
                                       ],
                                     YMLNewLine,
                                     YMLMapping
                                       "coocoo"
                                       [ YMLNewLine,
                                         YMLMapping "e" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine
                                       ],
                                     YMLNewLine
                                   ],
                                 YMLMapping "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                               ]
                           ]
                       ]

    describe "updateScalarAtPath" $ do
      let doc =
            [ YMLNewLine,
              YMLMapping "a" [YMLWSSpace, YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "hello world"]],
              YMLNewLine,
              YMLNewLine,
              YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
            ]

      context "when yaml doc contains a scalar value at path" $ do
        it "updates the value at that path" $ do
          let (isUpdated, newDoc) = updateScalarAtPath ["a", "b"] (const $ ScalarSingleQuote "Fuck you") doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLNewLine,
                         YMLMapping "a" [YMLWSSpace, YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "Fuck you"]],
                         YMLNewLine,
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]

      context "when yaml doc contains a non-scalar value at path" $ do
        it "does not update anything" $ do
          let (isUpdated, newDoc) = updateScalarAtPath ["a"] (const $ ScalarSingleQuote "Fuck you") doc
          isUpdated `shouldBe` True
          newDoc `shouldBe` doc

      context "when path does not exist" $ do
        it "inserts new path" $ do
          let (isUpdated, newDoc) = updateScalarAtPath ["a", "c"] (const $ ScalarSingleQuote "Fuck you") doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLNewLine,
                         YMLMapping
                           "a"
                           [ YMLWSSpace,
                             YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "hello world"],
                             YMLNewLine,
                             YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "Fuck you"],
                             YMLNewLine
                           ],
                         YMLNewLine,
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]
