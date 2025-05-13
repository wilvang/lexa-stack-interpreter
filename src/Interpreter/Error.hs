{-|
Module      : Interpreter.Error
Description : Defines the error types for the interpreter.

This module defines the `ParserError`, `ProgramError`, and `BError` types used to represent various errors
that can occur during the parsing and execution phases of the interpreter. These errors cover issues such as incomplete
quotations, expected values or symbols, and program execution failures like division by zero or empty list operations.

Data Types:
-------------
1. `ParserError`: Represents errors encountered during the parsing phase, such as incomplete quotations or lists.
2. `ProgramError`: Represents errors that occur during program execution, such as stack errors, unexpected symbols, and type mismatches.
3. `BError`: A sum type that encapsulates both `ParserError` and `ProgramError`, providing a unified error representation for the interpreter.

Example usage:

* To represent an error during parsing an incomplete string:
  `ParserError (IncompleteString "some string")`

* To represent a program error due to division by zero:
  `ProgramError DivisionByZero`

* To wrap a `ProgramError` in a `BError`:
  `BError (ProgramError DivisionByZero)`
-}

module Interpreter.Error (ParserError(..), ProgramError(..), BError(..)) where
  
import Interpreter.State (State(..))  

-- | Represents parser errors.
data ParserError
  = IncompleteQuotation String
  | IncompleteList String
  | IncompleteString String
  | UnexpectedEnd Char
  deriving (Show)

-- | Represents program execution errors.
data ProgramError =
     StackEmpty 
   | UnknownSymbol
   | ExpectedBool
   | ExpectedBoolOrNumber 
   | ExpectedString 
   | ExpectedEnumerable 
   | ExpectedList 
   | ExpectedQuotation State
   | ExpectedVariable State
   | ExpectedSymbol State
   | NotComparable 
   | DivisionByZero 
   | EmptyList 
   | ProgramFinishedWithMultipleValues 
   | NumberConversionError 
   deriving (Show)

-- | Represents all program errors
data BError = 
  ProgramError ProgramError 
  | ParserError ParserError 
  deriving (Show)