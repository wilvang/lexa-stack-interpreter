module Interpreter.Error (ParserError(..)) where

data ParserError
  = IncompleteQuotation [String]
  | IncompleteList [String]
  | IncompleteString [String]
  | UnexpectedEnd Char
  deriving Show
