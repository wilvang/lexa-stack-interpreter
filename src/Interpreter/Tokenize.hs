module Interpreter.Tokenize (splitPreserveTokens) where

import Data.Maybe (fromMaybe, listToMaybe)
import Interpreter.Error (ParserError(..))


-- | Convert an input string to a list of strings by handling blocks and strings
-- -- >>> splitPreserveTokens "1 2 3"
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
-- Left (IncompleteQuotation ["{","1","2"])
--
-- >>> splitPreserveTokens "[ 1 2 3"
-- Left (IncompleteList ["[","1","2","3"])
--
-- >>> splitPreserveTokens "\" unfinished string"
-- Left (IncompleteString ["\"","unfinished","string"])
--
splitPreserveTokens :: String -> Either ParserError [String]
splitPreserveTokens = fmap reverse . finalize . foldl collect ([], Nothing) . words
  where
    collect :: ([String], Maybe (String, [String])) -> String -> ([String], Maybe (String, [String]))
    collect (acc, Nothing) tok
      | tok == "{"  = (acc, Just ("}", [tok]))
      | tok == "["  = (acc, Just ("]", [tok]))
      | tok == "\"" = (acc, Just ("\"", [tok]))
      | otherwise   = (tok : acc, Nothing)

    collect (acc, Just (end, group)) tok
      | tok == end = (unwords (group ++ [tok]) : acc, Nothing)
      | otherwise  = (acc, Just (end, group ++ [tok]))

    finalize :: ([String], Maybe (String, [String])) -> Either ParserError [String]
    finalize (acc, Nothing) = Right acc
    finalize (_, Just (end, group)) = case end of
      "}"  -> Left (IncompleteQuotation group)
      "]"  -> Left (IncompleteList group)
      "\"" -> Left (IncompleteString group)
      _    -> Left (UnexpectedEnd (fromMaybe '?' (listToMaybe end)))
