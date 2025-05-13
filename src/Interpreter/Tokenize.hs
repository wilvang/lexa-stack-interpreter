-- | Module for tokenizing input strings into syntactic units while preserving grouping
--   of quotations, lists, and strings.
module Interpreter.Tokenize (splitPreserveTokens) where

import Data.Maybe (fromMaybe, listToMaybe)
import Interpreter.Error (ParserError(..), BError(..))

-- | Convert an input string to a list of strings by handling blocks and strings
-- >>> splitPreserveTokens "1 2 3"
-- Right ["1","2","3"]
--
-- >>> splitPreserveTokens "{ 1 2 } +"
-- Right ["{ 1 2 }","+"]
--
-- >>> splitPreserveTokens "[ a b c ] d"
-- Right ["[ a b c ]","d"]
--
-- >>> splitPreserveTokens " \" hello world \" x"
-- Right ["\" hello world \"","x"]
--
-- >>> splitPreserveTokens "{ 1 2"
-- Left (ParserError (IncompleteQuotation "{ 1 2"))
--
-- >>> splitPreserveTokens "[ 1 2 3"
-- Left (ParserError (IncompleteList "[ 1 2 3"))
--
-- >>> splitPreserveTokens "\" unfinished string"
-- Left (ParserError (IncompleteString "\" unfinished string"))
--
splitPreserveTokens :: String -> Either BError [String]
splitPreserveTokens = fmap reverse . finalize . foldl collect ([], Nothing) . words

-- | Collects tokens while grouping together syntactic blocks such as quotations (`{}`),
--   lists (`[]`), and strings (`"`). Starts a new group if one is opened.
collect :: ([String], Maybe ([String], [String])) -> String -> ([String], Maybe ([String], [String]))
collect (acc, Nothing) tok
  | tok == "{"  = (acc, Just (["}"], [tok]))
  | tok == "["  = (acc, Just (["]"], [tok]))
  | tok == "\"" = (acc, Just (["\""], [tok]))
  | otherwise   = (tok : acc, Nothing)

-- Handles a token after an unexpected empty end list (shouldn't happen but safe).
collect (acc, Just ([], group)) tok = (unwords (group ++ [tok]) : acc, Nothing)

-- Continues collecting tokens while inside a group.
collect (acc, Just (t@(end : rest), group)) tok 
  | isStringContext tok end      = (acc, Just (t, group ++ [tok]))
  | isValidNestedContext tok end = (acc, Just (end : t, group ++ [tok]))
  | tok == end && length t == 1  = (unwords (group ++ [tok]) : acc, Nothing)
  | tok == end                   = (acc, Just (rest, group ++ [tok]))
  | otherwise                    = (acc, Just (t, group ++ [tok]))

-- | Finalizes the token collection, checking for unclosed structures and 
--   reporting appropriate errors
finalize :: ([String], Maybe ([String], [String])) -> Either BError [String]
finalize (acc, Nothing) = Right acc
finalize (_, Just ([], _)) = Left . ParserError $ UnexpectedEnd '?'
finalize (_, Just (end:_, group)) = 
  case end of
    "}"  -> Left . ParserError $ IncompleteQuotation (unwords group)
    "]"  -> Left . ParserError $ IncompleteList (unwords group)
    "\"" -> Left . ParserError $ IncompleteString (unwords group)
    _    -> Left . ParserError $ UnexpectedEnd (fromMaybe '?' (listToMaybe end))

-- | Helper function to check if the current token is valid within the nested context 
--   of brackets or quotations.
isValidNestedContext :: String -> String -> Bool
isValidNestedContext tok end = tok == "{" && end == "}" || tok == "[" && end == "]"

-- | Checks whether we are inside a string context and the current token is not the closing quote.
-- Used to continue accumulating tokens until the closing quote is found.
isStringContext :: String -> String -> Bool
isStringContext tok end = end == "\"" && tok /= "\""