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
            [ YMLMapping 0 "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
              YMLNewLine,
              YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
              YMLNewLine,
              YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
            ]

      context "when yaml doc contains the key" $ do
        it "updates the value of the key" $ do
          let (isUpdated, newDoc) = updateKey "b" (const [YMLWSSpace, YMLScalar $ ScalarRawString "99"]) doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping 0 "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                         YMLNewLine,
                         YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "99"],
                         YMLNewLine,
                         YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]

      context "when yaml doc does not contains the key" $ do
        it "inserts new mapping" $ do
          let (isUpdated, newDoc) = updateKey "z" (const [YMLWSSpace, YMLScalar $ ScalarRawString "99"]) doc
          isUpdated `shouldBe` False
          newDoc
            `shouldBe` [ YMLMapping 0 "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                         YMLNewLine,
                         YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
                         YMLNewLine,
                         YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"],
                         YMLNewLine,
                         YMLMapping 0 "z" [YMLWSSpace, YMLScalar $ ScalarRawString "99"],
                         YMLNewLine
                       ]

    describe "updatePath" $ do
      let doc =
            [ YMLMapping
                0
                "root"
                [ YMLNewLine,
                  YMLMapping
                    0
                    "a"
                    [ YMLNewLine,
                      YMLMapping
                        0
                        "b"
                        [ YMLNewLine,
                          YMLMapping
                            0
                            "c"
                            [ YMLNewLine,
                              YMLMapping 0 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                              YMLNewLine,
                              YMLMapping 0 "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                              YMLNewLine,
                              YMLMapping 0 "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                              YMLNewLine
                            ]
                        ],
                      YMLMapping 0 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
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
                           0
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               0
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   0
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       0
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping 0 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping 0 "e" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine,
                                         YMLMapping 0 "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping 0 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
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
                           0
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               0
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   0
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       0
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping 0 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping 0 "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                                         YMLNewLine,
                                         YMLMapping 0 "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine,
                                         YMLMapping 0 "coocoo" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping 0 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                               ]
                           ]
                       ]

        it "inserts new mapping" $ do
          let updater = const [YMLWSSpace, YMLScalar $ ScalarRawString "999"]
          let (isUpdated, newDoc) = updatePath ["root", "a", "b", "coocoo", "e"] updater doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLMapping
                           0
                           "root"
                           [ YMLNewLine,
                             YMLMapping
                               0
                               "a"
                               [ YMLNewLine,
                                 YMLMapping
                                   0
                                   "b"
                                   [ YMLNewLine,
                                     YMLMapping
                                       0
                                       "c"
                                       [ YMLNewLine,
                                         YMLMapping 0 "d" [YMLWSSpace, YMLScalar $ ScalarRawString "123"],
                                         YMLNewLine,
                                         YMLMapping 0 "e" [YMLWSSpace, YMLScalar $ ScalarRawString "456"],
                                         YMLNewLine,
                                         YMLMapping 0 "f" [YMLWSSpace, YMLScalar $ ScalarRawString "789"],
                                         YMLNewLine
                                       ],
                                     YMLNewLine,
                                     YMLMapping
                                       0
                                       "coocoo"
                                       [ YMLNewLine,
                                         YMLMapping 0 "e" [YMLWSSpace, YMLScalar $ ScalarRawString "999"],
                                         YMLNewLine
                                       ],
                                     YMLNewLine
                                   ],
                                 YMLMapping 0 "foo" [YMLWSSpace, YMLScalar $ ScalarRawString "bar"]
                               ]
                           ]
                       ]

    describe "updateScalarAtPath" $ do
      let doc =
            [ YMLNewLine,
              YMLMapping 0 "a" [YMLWSSpace, YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "hello world"]],
              YMLNewLine,
              YMLNewLine,
              YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
            ]

      context "when yaml doc contains a scalar value at path" $ do
        it "updates the value at that path" $ do
          let (isUpdated, newDoc) = updateScalarAtPath ["a", "b"] (const $ ScalarSingleQuote "Fuck you") doc
          isUpdated `shouldBe` True
          newDoc
            `shouldBe` [ YMLNewLine,
                         YMLMapping 0 "a" [YMLWSSpace, YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "Fuck you"]],
                         YMLNewLine,
                         YMLNewLine,
                         YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
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
                           0
                           "a"
                           [ YMLWSSpace,
                             YMLMapping 0 "b" [YMLWSSpace, YMLScalar $ ScalarRawString "hello world"],
                             YMLNewLine,
                             YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarSingleQuote "Fuck you"],
                             YMLNewLine
                           ],
                         YMLNewLine,
                         YMLNewLine,
                         YMLMapping 0 "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]
