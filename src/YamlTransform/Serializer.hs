module YamlTransform.Serializer (serialize) where

import Data.Text (Text)
import YamlTransform.Types (Yaml (..))

tokenToText :: Yaml -> Text
tokenToText YMLNewLine = "\n"
tokenToText YMLWSSpace = " "
tokenToText YMLWSTab = "\t"
tokenToText (YMLComment c) = "#" <> c
tokenToText (YMLMapping k values) = k <> ":" <> serialize values
tokenToText (YMLSequenceItem values) = "-" <> serialize values
tokenToText (YMLScalar s) = s

serialize :: [Yaml] -> Text
serialize = foldr ((<>) . tokenToText) ""