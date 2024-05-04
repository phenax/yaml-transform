module Main where

import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (exitFailure)
import YamlTransform.Parser (Yaml, parse)
import YamlTransform.Serializer (serialize)
import YamlTransform.Transformer (getPath, updateScalarAtPath)
import YamlTransform.Types (YMLScalar (ScalarSingleQuote))

data Command
  = UpdateValue [Text] Text
  | PrintValue [Text]
  deriving (Show)

readYamlFile :: FilePath -> IO [Yaml]
readYamlFile path = do
  -- TODO: Handle stdin if - ?
  contents <- Text.pack <$> readFile path
  case parse contents of
    Left err -> do
      putStrLn "Error parsing yaml file::"
      print err
      exitFailure
    Right yaml -> pure yaml

writeYamlFile :: FilePath -> [Yaml] -> IO ()
writeYamlFile path yaml = do
  putStrLn $ Text.unpack $ serialize yaml

parseCommand :: [Text] -> (Maybe Command, [Text])
parseCommand ("update" : path : value : args) =
  (,args) $ Just $ UpdateValue (splitOn "." path) value
parseCommand ("print" : path : args) = (,args) $ Just $ PrintValue (splitOn "." path)
parseCommand args = (Nothing, args)

main :: IO ()
main = do
  args <- parseCommand <$> (map Text.pack <$> getArgs)
  case args of
    (Just (UpdateValue path value), [filePathText]) -> do
      let filePath = Text.unpack filePathText
      yaml <- readYamlFile filePath
      print (filePath, path, value)
      let (updated, result) = updateScalarAtPath path (const $ ScalarSingleQuote value) yaml
      writeYamlFile filePath result
      if updated
        then do putStrLn "Found and updated path"
        else putStrLn "Inserted new path"
    (Just (PrintValue path), [filePathText]) -> do
      let filePath = Text.unpack filePathText
      yaml <- readYamlFile filePath
      let value = maybe "" serialize $ getPath path yaml
      putStrLn . Text.unpack $ value
    _ -> do
      putStrLn "Invalid arguments"
      exitFailure
