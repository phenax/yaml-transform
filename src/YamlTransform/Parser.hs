module YamlTransform.Parser (parse, Yaml (..)) where

import Control.Applicative (optional)
import Control.Monad (join, unless, void, when)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified YamlTransform.Parser.Util as P
import YamlTransform.Types (Yaml (..))

undelimitedInlineString :: P.Parser Yaml
undelimitedInlineString =
  YMLScalar <$> P.takeUntilParsable endOfInlineValue
  where
    endOfInlineValue =
      P.choice
        [ P.endOfLine,
          P.endOfInput,
          void $ whitespaceP *> P.char '#',
          void $ P.satisfy (`elem` ("[]{}" :: String))
        ]

delimitedInlineStringP :: P.Parser Yaml
delimitedInlineStringP = YMLScalar <$> P.choice [quoted '\'', quoted '"']
  where
    quoted c = P.char c *> P.takeWhile1 (/= c) <* P.char c

numberP :: P.Parser Yaml
numberP = YMLScalar . Text.pack . show <$> P.double

inlineScalarValueP :: P.Parser Yaml
inlineScalarValueP = P.choice [delimitedInlineStringP, numberP, undelimitedInlineString]

nonRawScalarValueP :: P.Parser Yaml
nonRawScalarValueP = P.choice [delimitedInlineStringP, numberP]

inlineSequenceP :: P.Parser Yaml
inlineSequenceP = do
  values <- P.char '[' *> P.sepBy inlineValueP (P.char ',') <* P.char ']'
  pure $ YMLInlineSequence values
  where
    inlineValueP = P.concatListP [ignorablesP, pure <$> nonRawScalarValueP, ignorablesP]
    ignorablesP = P.concatListP [whitesP, fromMaybe [] <$> optional endOfLineIgnorableP, whitesP]
    whitesP = P.many' (P.choice [whitespaceP, newlineP])

inlineYmlValueP :: P.Parser [Yaml]
inlineYmlValueP = pure <$> P.choice [inlineSequenceP, inlineScalarValueP]

identifierP :: P.Parser Text
identifierP = P.choice [P.string "<<", P.takeWhile1 (P.inClass "a-zA-Z0-9-_$")]

indentCheckP :: Int -> Bool -> P.Parser [Yaml]
indentCheckP parentIndent allowSameLevel = do
  whitespaces <- P.many' indentWhitespaceP
  let indent = length whitespaces
  when (indent < parentIndent || (indent == parentIndent && not allowSameLevel)) $
    fail "Invalid indent level"
  pure whitespaces

mappingP :: Int -> Int -> P.Parser [Yaml]
mappingP parentIndent prefixIndent = do
  whitespaces <- indentCheckP (parentIndent - prefixIndent) False
  let indent = length whitespaces + prefixIndent
  key <- identifierP <* P.char ':'
  (whitespaces ++) . pure . YMLMapping key <$> valueP indent
  where
    valueP indent = P.choice [objectValueP indent, inlineValueP]
    objectValueP indent = P.concatListP [endOfLineIgnorableP, yamlP indent 0]
    inlineValueP = P.concatListP [P.many1 whitespaceP, inlineYmlValueP, inlineEndP]
    inlineEndP = fromMaybe [] <$> optional (P.lookAhead whitespaceP *> endOfLineIgnorableP)

sequenceItemP :: Int -> P.Parser [Yaml]
sequenceItemP parentIndent = do
  whitespaces <- indentCheckP parentIndent False
  let indent = length whitespaces
  values <- P.char '-' *> P.choice [yamlP indent indent, inlineValueP]
  pure $ whitespaces ++ [YMLSequenceItem values]
  where
    inlineValueP = P.concatListP [P.many1 whitespaceP, inlineYmlValueP, inlineEndP]
    inlineEndP = fromMaybe [] <$> optional (P.lookAhead whitespaceP *> endOfLineIgnorableP)

newlineP :: P.Parser Yaml
newlineP = YMLNewLine <$ P.endOfLine

indentWhitespaceP :: P.Parser Yaml
indentWhitespaceP = YMLWSSpace <$ P.space

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
  P.char '#'
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
          pure <$> inlineSequenceP,
          fullCommentP parentIndent,
          sequenceItemP parentIndent,
          mappingP parentIndent prefixIndent
        ]

leftOverP :: P.Parser ()
leftOverP = do
  rest <- P.takeWhile (const True)
  unless (Text.null rest) $ fail $ "Error at: " ++ show rest
  P.endOfInput

parse :: Text -> Either String [Yaml]
parse = P.parseOnly (yamlP (-1) 0 <* leftOverP)
