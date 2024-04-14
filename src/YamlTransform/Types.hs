module YamlTransform.Types where

import Data.Text (Text)

data Yaml
  = YMLMapping Text [Yaml]
  | YMLSequenceItem [Yaml]
  | YMLScalar Text -- TODO: Add scalar type
  | -- | YMLAlias Text -- TODO: Alias
    YMLComment Text
  | YMLWSSpace
  | YMLWSTab
  | YMLNewLine
  deriving (Show, Eq)
