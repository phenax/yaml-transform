module YamlTransform.Parser (parse, Yaml (..)) where

import Control.Applicative (optional)
import Control.Arrow ((&&&))
import Control.Monad (join, unless, void, when)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, cons)
import qualified Data.Text as Text
import qualified Debug.Trace as Debug

data Yaml
  = YMLMapping Text [Yaml]
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
  P.lookAhead P.anyChar >>= (Debug.traceM . ("[Next char] " ++) . show)

diverge :: (a -> b) -> (a -> c) -> a -> (b, c)
diverge = (&&&)

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
  (indent, whitespaces) <- diverge length id <$> P.many' whitespaceP
  when (indent <= currentIndent) $ fail "Invalid indent level"
  key <- identifierP <* P.char ':'
  (whitespaces ++) . pure . YMLMapping key <$> valueP indent
  where
    valueP indent = P.choice [objectValueP indent, inlineValueP]
    objectValueP indent = do
      comment <- endOfLineIgnorableP
      value <- yamlP indent
      pure $ comment ++ value
    inlineValueP = do
      spaces <- P.many1 whitespaceP
      value <- inlineScalarP
      comment <- fromMaybe [] <$> optional (P.lookAhead whitespaceP >> endOfLineIgnorableP)
      pure $ spaces ++ [value] ++ comment

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

endOfLineIgnorableP :: P.Parser [Yaml]
endOfLineIgnorableP = do
  spaces' <- P.many' whitespaceP
  comments <- maybeToList <$> optional commentP
  newlines <- P.many' newlineP
  pure $ spaces' ++ comments ++ newlines

commentP :: P.Parser Yaml
commentP = do
  P.char '#' >> P.lookAhead whitespaceCharP
  YMLComment <$> P.takeWhile1 (not . P.isEndOfLine)

yamlP :: Int -> P.Parser [Yaml]
yamlP level = join <$> P.many1 yamlPart
  where
    -- inlineScalarP
    yamlPart = P.choice [P.many1 newlineP, P.many1 commentP, mappingP level]

leftOverP :: P.Parser ()
leftOverP = do
  rest <- P.takeWhile (const True)
  unless (Text.null rest) $ fail $ "Error at: " ++ show rest
  P.endOfInput

parse :: Text -> Either String [Yaml]
parse = P.parseOnly (yamlP (-1) <* leftOverP)
