module YamlTransform.Types where

import Data.Text (Text)

data YMLScalar
  = ScalarSingleQuote Text
  | ScalarDoubleQuote Text
  | ScalarRawString Text
  deriving (Show, Eq)

data Yaml
  = YMLMapping Int Text [Yaml]
  | YMLSequenceItem [Yaml]
  | YMLScalar YMLScalar
  | YMLInlineSequence [[Yaml]]
  | YMLComment Text
  | YMLWSSpace
  | YMLWSTab
  | YMLNewLine
  | YMLAnchor Text
  deriving (Show, Eq)
