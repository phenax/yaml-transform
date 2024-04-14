module YamlTransform.Parser (parse, Yaml (..)) where

import Control.Applicative (optional)
import Control.Monad (join, unless, void, when)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, cons)
import qualified Data.Text as Text
import qualified Debug.Trace as Debug
import YamlTransform.Types (Yaml (..))

debugNextChar :: P.Parser ()
debugNextChar =
  P.lookAhead P.anyChar >>= (Debug.traceM . ("[Next char] " ++) . show)

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
  YMLScalar <$> takeUntilParsable endOfInlineValue
  where
    endOfInlineValue =
      P.choice
        [ P.endOfLine,
          P.endOfInput,
          void $ whitespaceP >> P.char '#' >> whitespaceP
        ]

identifierP :: P.Parser Text
identifierP = P.takeWhile1 (P.inClass "a-zA-Z0-9-_$")

mappingP :: Int -> Int -> P.Parser [Yaml]
mappingP parentIndent prefixIndent = do
  whitespaces <- P.many' whitespaceP
  let indent = length whitespaces + prefixIndent
  when (indent <= parentIndent) $ fail "Invalid indent level"
  key <- identifierP <* P.char ':'
  (whitespaces ++) . pure . YMLMapping key <$> valueP indent
  where
    valueP indent = P.choice [objectValueP indent, inlineValueP]
    objectValueP indent = do
      comment <- endOfLineIgnorableP
      value <- yamlP indent 0
      pure $ comment ++ value
    inlineValueP = do
      spaces <- P.many1 whitespaceP
      value <- inlineScalarP
      comment <- fromMaybe [] <$> optional (P.lookAhead whitespaceP >> endOfLineIgnorableP)
      pure $ spaces ++ [value] ++ comment

sequenceP :: Int -> P.Parser [Yaml]
sequenceP parentIndent = do
  whitespaces <- P.many' whitespaceP
  let indent = length whitespaces
  when (indent <= parentIndent) $ fail "Invalid indent level"
  P.char '-'
  values <- P.choice [yamlP indent indent, inlineValueP]
  pure $ whitespaces ++ [YMLSequenceItem values]
  where
    inlineValueP = do
      spaces <- P.many1 whitespaceP
      value <- inlineScalarP
      comment <-
        fromMaybe []
          <$> optional (P.lookAhead whitespaceP >> endOfLineIgnorableP)
      pure $ spaces ++ [value] ++ comment

newlineP :: P.Parser Yaml
newlineP = YMLNewLine <$ P.endOfLine

whitespaceP :: P.Parser Yaml
whitespaceP =
  P.choice [YMLWSSpace <$ P.char ' ', YMLWSTab <$ P.char '\t']

endOfLineIgnorableP :: P.Parser [Yaml]
endOfLineIgnorableP = do
  spaces' <- P.many' whitespaceP
  comments <- maybeToList <$> optional commentP
  newlines <- P.many' newlineP
  pure $ spaces' ++ comments ++ newlines

commentP :: P.Parser Yaml
commentP = do
  P.char '#' >> P.lookAhead whitespaceP
  YMLComment <$> P.takeWhile1 (not . P.isEndOfLine)

fullCommentP :: Int -> P.Parser [Yaml]
fullCommentP parentIndent = do
  whitespaces <- P.many' whitespaceP
  let indent = length whitespaces
  when (indent <= parentIndent) $ fail "Invalid indent level"
  comment <- commentP
  pure $ whitespaces ++ [comment]

yamlP :: Int -> Int -> P.Parser [Yaml]
yamlP parentIndent prefixIndent = join <$> P.many1 yamlPart
  where
    yamlPart =
      P.choice
        [ P.many1 newlineP,
          fullCommentP parentIndent,
          sequenceP parentIndent,
          mappingP parentIndent prefixIndent
        ]

leftOverP :: P.Parser ()
leftOverP = do
  rest <- P.takeWhile (const True)
  unless (Text.null rest) $ fail $ "Error at: " ++ show rest
  P.endOfInput

parse :: Text -> Either String [Yaml]
parse = P.parseOnly (yamlP (-1) 0 <* leftOverP)
