module YamlTransform.Parser where

import Control.Applicative (optional)
import Control.Monad (void)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.Maybe (maybeToList)
import Data.Text (Text, cons)

data WhitespaceType = WSTab | WSSpace
  deriving (Show, Eq)

data Yaml
  = YMLMapping Text [Yaml]
  | YMLSequence [Yaml]
  | YMLScalar Text -- TODO: Add scalar type
  | YMLAlias Text
  | YMLComment Text
  | YMLWhitespace Int WhitespaceType
  | YMLNewLine
  deriving (Show, Eq)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

takeUntilParsable :: P.Parser a -> P.Parser Text
takeUntilParsable p = do
  t <- P.lookAhead $ optional p
  case t of
    Just _ -> pure ""
    Nothing -> do
      c <- P.anyChar
      cons c <$> takeUntilParsable p

inlineScalarP :: P.Parser Yaml
inlineScalarP =
  YMLScalar <$> takeUntilParsable endOfInlineP
  where
    endOfInlineP =
      P.choice
        [ P.endOfLine,
          P.endOfInput,
          void $ whitespaceCharP >> P.char '#' >> whitespaceCharP
        ]

identifierP :: P.Parser Text
identifierP = P.takeWhile1 (P.inClass "a-zA-Z0-9")

mappingP :: P.Parser Yaml
mappingP = do
  key <- identifierP <* P.char ':'
  YMLMapping key <$> valueP
  where
    valueP = inlineValueP
    inlineValueP = do
      spaces <- P.many1 whitespaceP
      v <- inlineScalarP
      spaces' <- P.many' whitespaceP
      comment <- maybeToList <$> optional commentP
      pure $ spaces ++ [v] ++ spaces' ++ comment

whitespaceCharP :: P.Parser Char
whitespaceCharP = P.satisfy $ P.inClass " \t"

whitespaceP :: P.Parser Yaml
whitespaceP = P.choice [indentSpaceP, indentTabP]
  where
    indentSpaceP = do
      spaces <- P.many1 (P.char ' ')
      pure . YMLWhitespace (length spaces) $ WSSpace
    indentTabP = do
      tabs <- P.many1 (P.char '\t')
      pure . YMLWhitespace (length tabs) $ WSTab

commentP :: P.Parser Yaml
commentP = do
  P.char '#' >> P.lookAhead whitespaceCharP
  YMLComment <$> P.takeWhile1 (not . P.isEndOfLine)

yamlP :: P.Parser [Yaml]
yamlP = P.many' $ P.choice [whitespaceP, mappingP, commentP] -- inlineScalarP

parse :: Text -> Either String [Yaml]
parse = P.parseOnly (yamlP <* P.endOfInput)
