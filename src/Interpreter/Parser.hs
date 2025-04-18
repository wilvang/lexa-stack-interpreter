module Interpreter.Parser where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Interpreter.Types
import Interpreter.Tokenize (splitPreserveTokens)
port Interpreter.Error (ParserError)


-- | Convert an input string into a list of tokens by splitting it on whitespace.
-- The function uses 'splitPreserveTokens' to split the input, and then applies 'parseToken' to each string.
-- Returns an 'Either' with a 'ParserError' on failure or a list of 'Token' on success.
parseTokens :: String -> Either ParserError [Token]
parseTokens = fmap (fmap parseToken) . splitPreserveTokens

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

-- | Try to parse an operator by looking it up in a predefined table of operators.
maybeParseOp :: String -> Maybe Token
maybeParseOp s = TokOp <$> M.lookup s opTable
  where
    opTable = M.fromList
      [ ("+", OpAdd)
      , ("-", OpSub)
      , ("*", OpMul)
      , ("/", OpDiv)
      , ("div", OpIDiv)
      , ("<", OpLT)
      , (">", OpGT)
      , ("==", OpEQ)
      , ("&&", OpAnd)
      , ("||", OpOr)
      , ("not", OpNot)
      , ("exec", OpExec)
      , ("if", OpIf)
      , ("times", OpTimes)
      , (":=", OpAssign)
      , ("fun", OpFun)
      ]

-- | Try to parse an integer from a string.
-- Returns a token wrapped in 'VInt' if successful.
maybeParseInt :: String -> Maybe Token
maybeParseInt s = (TokVal . VInt) <$> (readMaybe s :: Maybe Int)

-- | Try to parse a floating point number from a string.
-- Returns a token wrapped in 'VFloat' if successful.
maybeParseFloat :: String -> Maybe Token
maybeParseFloat s = (TokVal . VFloat) <$> (readMaybe s :: Maybe Double)

-- | Try to parse a boolean value from a string.
-- Accepts only "True" or "False" (case-sensitive).
maybeParseBool :: String -> Maybe Token
maybeParseBool "True"  = Just (TokVal (VBool True))
maybeParseBool "False" = Just (TokVal (VBool False))
maybeParseBool _       = Nothing

-- | Try to parse a context-wrapped value such as a string, list, or quotation.
-- It checks if the input string is wrapped in valid delimiters (e.g., double quotes for a string,
-- square brackets for a list, or curly braces for a quotation).
-- If the string is wrapped appropriately, it returns a corresponding 'Token'.
-- Otherwise, it returns 'Nothing'.
maybeParseContext :: String -> Maybe Token
maybeParseContext s
  | isWrapped ('"', '"') s = (Just . TokVal . VString . unwrap) s
  | isWrapped ('[', ']') s = parseListContext s
  | isWrapped ('{', '}') s = parseQuotationContext s
  | otherwise              = Nothing

-- | Check if a string is wrapped by the given opening and closing characters.
-- The function checks if the first and last characters of the string match
-- the specified 'open' and 'close' characters. Returns 'True' if they do, 'False' otherwise.
isWrapped :: (Char, Char) -> String -> Bool
isWrapped (open, close) s = case uncons s of
  Just (first, rest) -> not (null rest) && last s == close && first == open
  Nothing            -> False

-- | Remove the first and last character from a string.
-- Useful for unwrapping context-delimited strings like quotes or lists.
unwrap :: String -> String
unwrap = init . drop 1

-- | Parse a string wrapped in curly braces as a quotation context.
-- It treats the content inside the braces as a series of tokens, creating a 'TokVal' of 'VQuotation'.
parseQuotationContext :: String -> Maybe Token
parseQuotationContext =
  fmap (TokVal . VQuotation) . traverse (Just . parseToken) . words . unwrap

-- | Parse a string wrapped in square brackets as a list context.
-- It treats the content inside the brackets as a series of tokens and converts them to 'VList' values.
parseListContext :: String -> Maybe Token
parseListContext =
  fmap (TokVal . VList) . traverse extractValue . map parseToken . words . unwrap
  where
    extractValue (TokVal v) = Just v
    extractValue _          = Nothing
