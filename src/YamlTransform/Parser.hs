module YamlTransform.Parser where

import Control.Applicative (optional)
import Control.Monad (join, void, when)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.Maybe (maybeToList)
import Data.Text (Text, cons)
import qualified Debug.Trace as Debug

data WhitespaceType = WSTab | WSSpace
  deriving (Show, Eq)

data Yaml
  = YMLMapping Int Text [Yaml]
  | YMLSequence [Yaml]
  | YMLScalar Text -- TODO: Add scalar type
  | YMLAlias Text
  | YMLComment Text
  | YMLWSSpace
  | YMLWSTab
  | YMLNewLine
  deriving (Show, Eq)

debugNextChar :: P.Parser ()
debugNextChar =
  P.lookAhead P.anyChar >>= (Debug.traceM . ("Next char: " ++) . show)

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
identifierP = P.takeWhile1 (P.inClass "a-zA-Z0-9-_$")

mappingP :: Int -> P.Parser [Yaml]
mappingP currentIndent = do
  whitespaces <- P.many' whitespaceP
  let indent = length whitespaces
  when (indent <= currentIndent) $ fail "Invalid indent level"
  key <- identifierP <* P.char ':'
  (whitespaces ++) . pure . YMLMapping indent key <$> valueP indent
  where
    valueP indent' = P.choice [objectValueP indent', inlineValueP]
    objectValueP indent' = do
      ignorablePrefix <- P.many' $ P.choice [whitespaceP, commentP]
      newlines <- P.many1 newlineP
      value <- yamlP indent'
      pure $ ignorablePrefix ++ newlines ++ value
    inlineValueP = do
      spaces <- P.many1 whitespaceP
      value <- inlineScalarP
      spaces' <- P.many' whitespaceP
      comment <- maybeToList <$> optional commentP
      pure $ spaces ++ [value] ++ spaces' ++ comment

whitespaceCharP :: P.Parser Char
whitespaceCharP = P.satisfy $ P.inClass " \t"

newlineP :: P.Parser Yaml
newlineP = YMLNewLine <$ P.endOfLine

whitespaceP :: P.Parser Yaml
whitespaceP =
  P.choice
    [ YMLWSSpace <$ P.char ' ',
      YMLWSTab <$ P.char '\t'
    ]

commentP :: P.Parser Yaml
commentP = do
  P.char '#' >> P.lookAhead whitespaceCharP
  YMLComment <$> P.takeWhile1 (not . P.isEndOfLine)

yamlP :: Int -> P.Parser [Yaml]
yamlP level = join <$> P.many' yamlPart
  where
    -- inlineScalarP
    yamlPart = P.choice [mappingP level, P.many1 commentP, P.many1 newlineP]

parse :: Text -> Either String [Yaml]
parse = P.parseOnly (yamlP (-1) <* P.endOfInput)
