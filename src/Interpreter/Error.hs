module Interpreter.Error (ParserError(..), ProgramError(..), BError(..)) where  

-- | Represents parser errors.
data ParserError
  = IncompleteQuotation String
  | IncompleteList String
  | IncompleteString String
  | UnexpectedEnd Char
  deriving (Eq, Show)


-- | Represents program execution errors.
data ProgramError =
     StackEmpty
   | UnknownSymbol
   | ExpectedBool
   | ExpectedBoolOrNumber
   | ExpectedString
   | ExpectedEnumerable
   | ExpectedQuotation
   | ExpectedList
   | ExpectedVariable
   | ExpectedSymbol
   | NotComparable
   | DivisionByZero
   | EmptyList
   | ProgramFinishedWithMultipleValues
   | NumberConversionError
   deriving (Eq, Show)

-- | Represents all program errors
data BError = 
  ProgramError ProgramError 
  | ParserError ParserError 
  deriving (Eq, Show)