module YamlTransform.Serializer (serialize) where

import Data.Text (Text, intercalate)
import YamlTransform.Types (YMLScalar (..), Yaml (..))

tokenToText :: Yaml -> Text
tokenToText YMLNewLine = "\n"
tokenToText YMLWSSpace = " "
tokenToText YMLWSTab = "\t"
tokenToText (YMLComment c) = "#" <> c
tokenToText (YMLMapping _ k values) = k <> ":" <> serialize values
tokenToText (YMLSequenceItem _ values) = "-" <> serialize values
tokenToText (YMLInlineSequence values) = "[" <> intercalate "," (serialize <$> values) <> "]"
tokenToText (YMLAnchor s) = "&" <> s
tokenToText (YMLScalar (ScalarRawString s)) = s
tokenToText (YMLScalar (ScalarSingleQuote s)) = "'" <> s <> "'"
tokenToText (YMLScalar (ScalarDoubleQuote s)) = "\"" <> s <> "\""

serialize :: [Yaml] -> Text
serialize = foldr ((<>) . tokenToText) ""
