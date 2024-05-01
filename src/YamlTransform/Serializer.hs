module YamlTransform.Serializer (serialize) where

import Data.Text (Text, intercalate, pack)
import YamlTransform.Types (YMLScalar (..), Yaml (..))

tokenToText :: Yaml -> Text
tokenToText YMLNewLine = "\n"
tokenToText YMLWSSpace = " "
tokenToText YMLWSTab = "\t"
tokenToText (YMLComment c) = "#" <> c
tokenToText (YMLMapping k values) = k <> ":" <> serialize values
tokenToText (YMLSequenceItem values) = "-" <> serialize values
tokenToText (YMLInlineSequence values) = "[" <> intercalate "," (serialize <$> values) <> "]"
tokenToText (YMLAnchor s) = "&" <> s
tokenToText (YMLScalar (ScalarRawString s)) = s
tokenToText (YMLScalar (ScalarSingleQuote s)) = "'" <> s <> "'"
tokenToText (YMLScalar (ScalarDoubleQuote s)) = "\"" <> s <> "\""
tokenToText (YMLScalar (ScalarNumber n)) = pack $ show n
tokenToText (YMLScalar s) = pack $ show s

serialize :: [Yaml] -> Text
serialize = foldr ((<>) . tokenToText) ""
