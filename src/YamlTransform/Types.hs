module YamlTransform.Types where

import Data.Text (Text)

-- data YMLString
--   = StrSingleQuote Text
--   | StrDoubleQuote Text
--   | StrRaw Text
--   deriving (Show, Eq)
--
-- data YMLScalar
--   = YMLString YMLString
--   | YMLInt Int
--   | YMLFloat Float
--   | YMLBool Bool
--   | YMLNull
--   deriving (Show, Eq)

data Yaml
  = YMLMapping Text [Yaml]
  | YMLSequenceItem [Yaml]
  | YMLScalar Text
  | YMLInlineSequence [[Yaml]]
  | YMLComment Text
  | YMLWSSpace
  | YMLWSTab
  | YMLNewLine
  | YMLAnchor Text
  deriving (Show, Eq)
