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
import YamlTransform.Transformer (updateKey, updatePath)
import YamlTransform.Types (YMLScalar (ScalarRawString))

test :: SpecWith ()
test = do
  describe "YamlTransform.Transformer" $ do
    describe "updateKey" $ do
      context "when yaml doc contains the key" $ do
        let doc =
              [ YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                YMLNewLine,
                YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "2"],
                YMLNewLine,
                YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
              ]
        it "updates the value of the key" $ do
          let (found, updated) = updateKey "b" (const [YMLWSSpace, YMLScalar $ ScalarRawString "99"]) doc
          found `shouldBe` True
          updated
            `shouldBe` [ YMLMapping "a" [YMLWSSpace, YMLScalar $ ScalarRawString "1"],
                         YMLNewLine,
                         YMLMapping "b" [YMLWSSpace, YMLScalar $ ScalarRawString "99"],
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar $ ScalarRawString "3"]
                       ]

    describe "updatePath" $ do
      context "when yaml doc contains the key" $ do
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
                                YMLMapping
                                  "d"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarRawString "123"
                                  ],
                                YMLNewLine,
                                YMLMapping
                                  "e"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarRawString "456"
                                  ],
                                YMLNewLine,
                                YMLMapping
                                  "f"
                                  [ YMLWSSpace,
                                    YMLScalar $ ScalarRawString "789"
                                  ],
                                YMLNewLine
                              ]
                          ],
                        YMLMapping
                          "foo"
                          [ YMLWSSpace,
                            YMLScalar $ ScalarRawString "bar"
                          ]
                      ]
                  ]
              ]
        it "updates the value of the key" $ do
          let updater = const [YMLWSSpace, YMLScalar $ ScalarRawString "999"]
          let (found, updated) = updatePath ["root", "a", "b", "c", "e"] updater doc
          found `shouldBe` True
          updated
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
                                         YMLMapping
                                           "d"
                                           [ YMLWSSpace,
                                             YMLScalar $ ScalarRawString "123"
                                           ],
                                         YMLNewLine,
                                         YMLMapping
                                           "e"
                                           [ YMLWSSpace,
                                             YMLScalar $ ScalarRawString "999"
                                           ],
                                         YMLNewLine,
                                         YMLMapping
                                           "f"
                                           [ YMLWSSpace,
                                             YMLScalar $ ScalarRawString "789"
                                           ],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping
                                   "foo"
                                   [ YMLWSSpace,
                                     YMLScalar $ ScalarRawString "bar"
                                   ]
                               ]
                           ]
                       ]
