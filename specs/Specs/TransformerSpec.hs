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

test :: SpecWith ()
test = do
  describe "YamlTransform.Transformer" $ do
    describe "updateKey" $ do
      context "when yaml doc contains the key" $ do
        let doc =
              [ YMLMapping "a" [YMLWSSpace, YMLScalar "1"],
                YMLNewLine,
                YMLMapping "b" [YMLWSSpace, YMLScalar "2"],
                YMLNewLine,
                YMLMapping "c" [YMLWSSpace, YMLScalar "3"]
              ]
        it "updates the value of the key" $ do
          let (found, updated) = updateKey "b" (const [YMLWSSpace, YMLScalar "99"]) doc
          found `shouldBe` True
          updated
            `shouldBe` [ YMLMapping "a" [YMLWSSpace, YMLScalar "1"],
                         YMLNewLine,
                         YMLMapping "b" [YMLWSSpace, YMLScalar "99"],
                         YMLNewLine,
                         YMLMapping "c" [YMLWSSpace, YMLScalar "3"]
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
                                    YMLScalar "123"
                                  ],
                                YMLNewLine,
                                YMLMapping
                                  "e"
                                  [ YMLWSSpace,
                                    YMLScalar "456"
                                  ],
                                YMLNewLine,
                                YMLMapping
                                  "f"
                                  [ YMLWSSpace,
                                    YMLScalar "789"
                                  ],
                                YMLNewLine
                              ]
                          ],
                        YMLMapping
                          "foo"
                          [ YMLWSSpace,
                            YMLScalar "bar"
                          ]
                      ]
                  ]
              ]
        it "updates the value of the key" $ do
          let updater = const [YMLWSSpace, YMLScalar "999"]
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
                                             YMLScalar "123"
                                           ],
                                         YMLNewLine,
                                         YMLMapping
                                           "e"
                                           [ YMLWSSpace,
                                             YMLScalar "999"
                                           ],
                                         YMLNewLine,
                                         YMLMapping
                                           "f"
                                           [ YMLWSSpace,
                                             YMLScalar "789"
                                           ],
                                         YMLNewLine
                                       ]
                                   ],
                                 YMLMapping
                                   "foo"
                                   [ YMLWSSpace,
                                     YMLScalar "bar"
                                   ]
                               ]
                           ]
                       ]
