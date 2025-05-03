module Interpreter.Tokenize (splitPreserveTokens) where

import Data.Maybe (fromMaybe, listToMaybe)
import Interpreter.Error (ParserError(..), BError(..))


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

    finalize :: ([String], Maybe (String, [String])) -> Either BError [String]
    finalize (acc, Nothing) = Right acc
    finalize (_, Just (end, group)) = case end of
      "}"  -> Left . ParserError $ IncompleteQuotation (unwords group)  
      "]"  -> Left . ParserError $  IncompleteList (unwords group)  
      "\"" -> Left . ParserError $  IncompleteString (unwords group)
      _    -> Left . ParserError $  UnexpectedEnd (fromMaybe '?' (listToMaybe end))
