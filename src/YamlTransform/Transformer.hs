module YamlTransform.Transformer (updateKey, updatePath, updateScalarAtPath, getPath) where

import Data.Text (Text)
import YamlTransform.Types (YMLScalar (ScalarRawString), Yaml (..))

getKey :: Text -> [Yaml] -> Maybe [Yaml]
getKey _ [] = Nothing
getKey key ((YMLMapping k values) : _)
  | k == key = Just values
getKey key (_ : ymls) = getKey key ymls

getPath :: [Text] -> [Yaml] -> Maybe [Yaml]
getPath [] ymls = Just ymls
getPath [key] ymls = getKey key ymls
getPath (key : keys) ymls = getKey key ymls >>= getPath keys

updateKey :: Text -> ([Yaml] -> [Yaml]) -> [Yaml] -> (Bool, [Yaml])
updateKey key updater [] = (False, [YMLNewLine, YMLMapping key $ updater [], YMLNewLine])
updateKey key updater [YMLNewLine] = (False, [YMLNewLine, YMLMapping key $ updater [], YMLNewLine])
updateKey key updater ((YMLMapping k values) : ymls)
  | k == key = (True, YMLMapping k (updater values) : ymls)
updateKey key updater (yml : ymls) = (found, yml : updated)
  where
    (found, updated) = updateKey key updater ymls

updatePath :: [Text] -> ([Yaml] -> [Yaml]) -> [Yaml] -> (Bool, [Yaml])
updatePath [] _ ymls = (False, ymls)
updatePath [key] updater ymls = updateKey key updater ymls
updatePath (key : keys) updater ymls = updateKey key updateInsideKey ymls
  where
    updateInsideKey doc = snd $ updatePath keys updater doc

updateScalarAtPath :: [Text] -> (YMLScalar -> YMLScalar) -> [Yaml] -> (Bool, [Yaml])
updateScalarAtPath path updater = updatePath path updateScalar
  where
    updateScalar [] = [YMLWSSpace, YMLScalar $ updater $ ScalarRawString ""]
    updateScalar yaml = mapScalarNode <$> yaml
    mapScalarNode (YMLScalar scalar) = YMLScalar $ updater scalar
    mapScalarNode n = n
