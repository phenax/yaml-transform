module YamlTransform.Transformer (updateKey, updatePath) where

import Data.Text (Text)
import YamlTransform.Types (Yaml (..))

updatePath :: [Text] -> ([Yaml] -> [Yaml]) -> [Yaml] -> (Bool, [Yaml])
updatePath [] _ ymls = (False, ymls)
updatePath [key] updater ymls = updateKey key updater ymls
updatePath (key : keys) updater ymls = updateKey key nextIter ymls
  where
    nextIter doc =
      let (_found, updated) = updatePath keys updater doc in updated

updateKey :: Text -> ([Yaml] -> [Yaml]) -> [Yaml] -> (Bool, [Yaml])
updateKey _ _ [] = (False, [])
updateKey key updater ((YMLMapping k values) : ymls)
  | k == key = (True, YMLMapping k (updater values) : ymls)
updateKey key updater (yml : ymls) = (found, yml : updated)
  where
    (found, updated) = updateKey key updater ymls
