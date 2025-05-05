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
   | ExpectTwoValues
   | ExpectedBool
   | ExpectedBoolOrNumber
   | ExpectedEnumerable
   | ExpectedQuotation
   | ExpectedList
   | ExpectedVariable
   | NotComparable
   | DivisionByZero
   | ProgramFinishedWithMultipleValues
   | MissingArgument
   | NumberConversionError
   deriving (Eq, Show)

-- | Represents all program errors
data BError = 
  ProgramError ProgramError 
  | ParserError ParserError 
  deriving (Eq, Show)