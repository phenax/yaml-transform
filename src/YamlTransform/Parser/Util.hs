module YamlTransform.Parser.Util where

import Control.Applicative (optional)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P
import Data.List (foldl')
import Data.Text (Text, cons)
import qualified Debug.Trace as Debug

debugNext :: (Show a) => P.Parser a -> P.Parser ()
debugNext p =
  P.lookAhead p >>= (Debug.traceM . ("[Next] " ++) . show)

debugNextChar :: P.Parser ()
debugNextChar = debugNext P.anyChar

concatListP :: [P.Parser [a]] -> P.Parser [a]
concatListP = foldl' concatListP' (pure [])
  where
    concatListP' p1 p2 = p1 >>= (\v -> (v ++) <$> p2)

takeUntilParsable :: P.Parser a -> P.Parser Text
takeUntilParsable p = do
  t <- P.lookAhead $ optional p
  case t of
    Just _ -> pure ""
    Nothing -> do
      c <- P.anyChar
      cons c <$> takeUntilParsable p
