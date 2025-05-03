module Interpreter.Error (ParserError(..), ProgramError(..)) where

-- | Represents parser errors.
data ParserError
  = IncompleteQuotation String
  | IncompleteList String
  | IncompleteString String
  | UnexpectedEnd Char
  deriving Show


-- | Represents program execution errors.
data ProgramError =
     StackEmpty
   | UnknownSymbol
   | ExpectedBool
   | ExpectedBoolOrNumber
   | ExpectedEnumerable
   | ExpectedQuotation
   | ExpectedList
   | ExpectedVariable
   | DivisionByZero
   | ProgramFinishedWithMultipleValues
   | NumberConversionError
     deriving (Eq, Show)

