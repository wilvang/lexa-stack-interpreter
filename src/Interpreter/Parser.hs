{-# LANGUAGE LambdaCase #-}
module Interpreter.Parser (parseTokens) where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Interpreter.Types
import Interpreter.Tokenize (splitPreserveTokens)
import Interpreter.Error (BError)

-- | Convert an input string into a list of tokens by splitting it on whitespace.
-- The function uses 'splitPreserveTokens' to split the input, and then applies 'parseToken' to each string.
-- Returns an 'Either' with a 'ParserError' on failure or a list of 'Token' on success.
--
-- >>>parseTokens ""
-- Right []
--
-- >>> parseTokens "1 2 +"
-- Right [1,2,+]
--
-- >>> parseTokens "\"hello\" True [ 1 2 ]"
-- Right ["hello",True,[ 1 2 ]]
--
-- >>> parseTokens "{ { 10 20 + } exec } exec"
-- Right [{ { 10 20 + } exec },exec]
-- >>> parseTokens "1 2 [ 2 3 4 ] { \" Oh no \""
-- Left (ParserError (IncompleteQuotation "{ \" Oh no \""))
parseTokens :: String -> Either BError [Token]
parseTokens = fmap (fmap parseToken) . splitPreserveTokens

-- | Parse a single string into a 'Token'. This function attempts to match the string to different token types
-- using a series of 'maybe' parsers. If none of the parsers succeed, the string is treated as a symbol.
-- Returns a 'Token' on success.
--
-- >>> parseToken "42"
-- 42
--
-- >>> parseToken "3.14"
-- 3.14
--
-- >>> parseToken "True"
-- True
--
-- >>> parseToken "\"hello\""
-- "hello"
--
-- >>> parseToken "+"
-- +
--
-- >>> parseToken "foobar"
-- foobar
parseToken :: String -> Token
parseToken s =
  fromMaybe (TokVal (VSymbol s)) $
    maybeParseOp s
    <|> maybeParseInt s
    <|> maybeParseFloat s
    <|> maybeParseBool s
    <|> maybeParseContext s

-- | Try to parse an operator by looking it up in a predefined table of operators.
-- Returns 'Just (TokOp op)' if a match is found, otherwise 'Nothing'.
--
-- >>> maybeParseOp "+"
-- Just +
--
-- >>> maybeParseOp "div"
-- Just div
--
-- >>> maybeParseOp "unknown"
-- Nothing
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
      , ("loop", OpLoop)
      , (":=", OpAssign)
      , ("fun", OpFun)
      , ("dup", OpDup)
      , ("swap", OpSwap)
      , ("pop", OpPop)
      , ("cons", OpCons)
      , ("append", OpAppend)
      , ("head", OpHead)
      , ("tail", OpTail)
      , ("empty", OpEmpty)
      , ("length", OpLength)
      , ("each", OpEach)
      , ("map", OpMap)
      , ("print", OpPrint)
      , ("read", OpRead)
      , ("parseInteger", OpParseInt)
      , ("parseFloat", OpParseFloat)
      , ("words", OpWords)
      , ("foldl", OpFoldl)
      ]

-- | Try to parse an integer from a string.
-- Returns a token wrapped in 'VInt' if successful.
--
-- >>> maybeParseInt "123"
-- Just 123
--
-- >>> maybeParseInt "abc"
-- Nothing
maybeParseInt :: String -> Maybe Token
maybeParseInt s = TokVal . VInt <$> (readMaybe s :: Maybe Integer)

-- | Try to parse a floating point number from a string.
-- Returns a token wrapped in 'VFloat' if successful.
--
-- >>> maybeParseFloat "3.14"
-- Just 3.14
--
-- >>> maybeParseFloat "42"
-- Just 42.0
--
-- >>> maybeParseFloat "nope"
-- Nothing
maybeParseFloat :: String -> Maybe Token
maybeParseFloat s = TokVal . VFloat <$> (readMaybe s :: Maybe Double)

-- | Try to parse a boolean value from a string.
-- Accepts only "True" or "False" (case-sensitive).
--
-- >>> maybeParseBool "True"
-- Just True
--
-- >>> maybeParseBool "False"
-- Just False
--
-- >>> maybeParseBool "true"
-- Nothing
maybeParseBool :: String -> Maybe Token
maybeParseBool "True"  = Just . TokVal . VBool $ True
maybeParseBool "False" = Just . TokVal . VBool $ False
maybeParseBool _       = Nothing


-- | Try to parse a context-wrapped value such as a string, list, or quotation.
-- It checks if the input string is wrapped in valid delimiters (e.g., double quotes for a string,
-- square brackets for a list, or curly braces for a quotation).
-- If the string is wrapped appropriately, it returns a corresponding 'Token'.
-- Otherwise, it returns 'Nothing'.
--
-- Example:
--
-- >>> maybeParseContext "\" Hello, world! \""
-- Just " Hello, world! "
--
-- >>> maybeParseContext "[1, 2, 3]"
-- Just [ 1, 2, 3 ]
--
-- >>> maybeParseContext "{ foo }"
-- Just { foo }
--
-- >>> maybeParseContext "{ 1 [ 1 2 3 ] + - }"
-- Just { 1 [ 1 2 3 ] + - }
--
-- >>> maybeParseContext "Hello"
-- Nothing
maybeParseContext :: String -> Maybe Token
maybeParseContext s
  | isWrapped ('"', '"') s = Just . TokVal . VString . unwords . words $ unwrap s
  | isWrapped ('[', ']') s = parseListContext s
  | isWrapped ('{', '}') s = parseQuotationContext s
  | otherwise              = Nothing


-- | Check if a string is wrapped by the given opening and closing characters.
-- The function checks if the first and last characters of the string match
-- the specified 'open' and 'close' characters. Returns 'True' if they do, 'False' otherwise.
--
-- Example:
--
-- >>> isWrapped ('"', '"') "\" Hello \""
-- True
--
-- >>> isWrapped ('[', ']') "[1, 2, 3]"
-- True
--
-- >>> isWrapped ('{', '}') "foo"
-- False
--
-- >>> isWrapped ('{', '}') "{ 1 4 6 + + *"
-- False
isWrapped :: (Char, Char) -> String -> Bool
isWrapped (open, close) s = case uncons s of
  Just (first, rest) -> not (null rest) && last s == close && first == open
  Nothing            -> False


-- | Remove the first and last character from a string.
-- Useful for unwrapping context-delimited strings like quotes or lists.
--
-- Example:
--
-- >>> unwrap "\"Hello, world!\""
-- "Hello, world!"
--
-- >>> unwrap "[1, 2, 3]"
-- "1, 2, 3"
--
-- >>> unwrap "{foo}"
-- "foo"
unwrap :: String -> String
unwrap = init . drop 1

-- | Parse a string wrapped in curly braces as a quotation context.
-- It treats the content inside the braces as a series of tokens, creating a 'TokVal' of 'VQuotation'.
--
-- Example:
--
-- >>> parseQuotationContext "{ foo }"
-- Just { foo }
--
-- >>> parseQuotationContext "{ bar baz }"
-- Just { bar baz }
--
-- >>> parseQuotationContext "{ [ \" hello \" \" world \" ] + - }"
-- Just { [ " hello " " world " ] + - }
parseQuotationContext :: String -> Maybe Token
parseQuotationContext str = 
  case parseTokens (unwrap str) of
    Right tokens -> Just . TokVal $ VQuotation tokens  -- If successful, wrap the result in VQuotation.
    Left _       -> Nothing  -- If there's an error, return Nothing.


-- | Parse a string wrapped in square brackets as a list context.
-- It treats the content inside the brackets as a series of tokens and converts them to 'VList' values.
--
-- Example:
--
-- >>> parseListContext "[1, 2, 3]"
-- Just [ 1, 2, 3 ]
--
-- >>> parseListContext "[\"apple\", \"banana\"]"
-- Just [ "apple", "banana" ]
--
-- >>> parseListContext "[ foo ]"
-- Just [ foo ]
--
-- >>> parseListContext "[ + + - * ]"
-- Nothing
--
-- >>> parseListContext "[ 1 2 - * ]"
-- Nothing
parseListContext :: String -> Maybe Token
parseListContext str =
  case parseTokens (unwrap str) of
    Right tokens ->
      traverse (\case TokVal v -> Just v; _ -> Nothing) tokens
        >>= \values -> Just (TokVal (VList values))
    Left _ -> Nothing