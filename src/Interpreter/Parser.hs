module Interpreter.Parser where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Interpreter.Types


-- | Convert an input string into a list of tokens by splitting 
-- it on whitespace.
parseTokens :: String -> [Token]
parseTokens = map parseToken . words

-- | Parse a single string into a 'Token'. This function 
-- attempts to match the string to different token types
parseToken :: String -> Token
parseToken s =
  fromMaybe (TokVal (VSymbol s)) $
    maybeParseOp s
    <|> maybeParseInt s
    <|> maybeParseFloat s
    <|> maybeParseBool s
    <|> maybeParseContext s
