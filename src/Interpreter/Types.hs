{-|
Module      : Interpreter.Types
Description : Defines the types and operations for the interpreter.

The module defines the core data types `Value`, `Op`, and `Token` used by the interpreter.
It includes the `Show` instance for these types, allowing for the display of values in a human-readable format.

Example usage for each type:

Data Types:
------------
1. `Value` represents values in the interpreter (integers, floats, booleans, strings, lists, quotations).
2. `Op` represents operations (like addition, subtraction, etc.).
3. `Token` is a wrapper for either a value or an operation.
-}

module Interpreter.Types (Value(..), Op(..), Token(..), sameConstructor) where

-- | Core data types

-- 'Value' represents all possible values that can appear on the stack.
data Value
  -- Primitive types
  = VInt Int           -- Integer value
  | VFloat Double      -- Floating-point value
  | VBool Bool         -- Boolean value (True or False)
  | VString String     -- A literal string value (e.g., "Hello")

  -- Composite Types
  | VList [Value]      -- A list of values (e.g., [1 2 3])
  | VQuotation [Token] -- A code block (quotation), a list of tokens to be executed

  -- Special types
  | VSymbol String     -- A symbol, typically used for variable or function names (e.g., `name` or `inc`)
  deriving (Eq)        -- Eq allows for comparison (e.g., checking equality between two values)

-- | Operations (Op) represent the various commands and operators in the language.
data Op
  -- Arithmetic Operations
  = OpAdd         -- Addition operator (x + y)
  | OpSub         -- Subtraction operator (x - y)
  | OpMul         -- Multiplication operator (x * y)
  | OpDiv         -- Floating point division operator (x / y)
  | OpIDiv        -- Integer division operator (x div y)

  -- Comparison Operators
  | OpLT          -- Less-than comparison operator (x < y)
  | OpGT          -- Greater-than comparison operator (x > y)
  | OpEQ          -- Equality comparison operator (x == y)

  -- Logical Operators
  | OpAnd         -- Logical AND operator (x && y)
  | OpOr          -- Logical OR operator (x || y)
  | OpNot         -- Logical NOT operator (not x)

  -- Control Flow Operators
  | OpExec        -- Execute a quotation (block of code)
  | OpIf          -- If statement (x if condition)
  | OpTimes       -- Repeat block n times
  | OpLoop        -- Loop through a block, based on a condition
  | OpAssign      -- Assignment operator (e.g., name := value)
  | OpFun         -- Function definition (e.g., fun inc { 1 + })

  -- Stack Manipulation
  | OpDup         -- Duplicate the top value on the stack
  | OpSwap        -- Swap the top two values on the stack
  | OpPop         -- Pop the top value from the stack

  -- List Operations
  | OpCons        -- Cons operator (prepend an item to a list)
  | OpAppend      -- Append two lists together
  | OpHead        -- Get the head of a list
  | OpTail        -- Get the tail of a list
  | OpEmpty       -- Check if a list is empty (return boolean)
  | OpLength      -- Get the length of a list
  | OpEach        -- Apply a function (quotation) to each item in a list
  | OpMap         -- Apply a function (quotation) to each item in a list and return a new list
  | OpFoldl       -- Fold left operator (list -> accumulator -> final value)

  -- I/O and Parsing
  | OpPrint       -- Print the top value from the stack
  | OpRead        -- Read an input (e.g., from a user or file)

  -- String parsing
  | OpParseInt    -- Parse an integer from input (e.g., string -> Integer)
  | OpParseFloat  -- Parse a floating point number from input (e.g., string -> Float)
  | OpWords       -- Convert a string into a list of words (split by space)  
  deriving (Eq)   -- Eq allows for comparison (e.g., checking equality between two operations)

-- | Tokens are either values or operations (commands).
-- A 'Token' represents an individual element of the language (either a value or an operation).
data Token
  = TokVal Value   -- A token holding a value (e.g., 10, "hello")
  | TokOp Op       -- A token holding an operation (e.g., +, :=, if)
  deriving (Eq)    -- Eq for comparison between two tokens

-- Custom Show instance for 'Value', for printing values.
instance Show Value where
  -- Primitive types
  show (VInt n)       = show n               -- Display an integer as is
  show (VFloat f)     = show f               -- Display a floating-point number as is
  show (VBool b)      = show b               -- Display a boolean as is (True or False)
  show (VString s)    = show s               -- Display a string as is (e.g., "hello")

  -- Composite Types
  show (VList xs)     = "[ " ++ unwords (show <$> xs) ++ " ]"  -- Display list of values, separated by space
  show (VQuotation q) = "{ " ++ unwords (show <$> q) ++ " }"   -- Display a quotation (code block) as space-separated tokens

  -- Special types
  show (VSymbol s)    = s                     -- Display the symbol as is (e.g., variable name 'name')

-- Custom Show instance for 'Op', for printing operations.
instance Show Op where
  -- Arithmetic Operations
  show OpAdd        = "+"      -- Addition operator (x + y)
  show OpSub        = "-"      -- Subtraction operator (x - y)
  show OpMul        = "*"      -- Multiplication operator (x * y)
  show OpDiv        = "/"      -- Floating point division operator (x / y)
  show OpIDiv       = "div"    -- Integer division operator (x div y)

  -- Comparison Operators
  show OpLT         = "<"      -- Less-than comparison operator (x < y)
  show OpGT         = ">"      -- Greater-than comparison operator (x > y)
  show OpEQ         = "=="     -- Equality comparison operator (x == y)

  -- Logical Operators
  show OpAnd        = "&&"     -- Logical AND operator (x && y)
  show OpOr         = "||"     -- Logical OR operator (x || y)
  show OpNot        = "not"    -- Logical NOT operator (not x)

  -- Control Flow Operators
  show OpExec       = "exec"   -- Execute a quotation (block of code)
  show OpIf         = "if"     -- If statement (x if condition)
  show OpTimes      = "times"  -- Repeat block n times
  show OpLoop       = "loop"   -- Loop through a block, based on a condition
  show OpAssign     = ":="     -- Assignment operator (e.g., name := value)
  show OpFun        = "fun"    -- Function definition (e.g., fun inc { 1 + })

  -- Stack Manipulation
  show OpDup        = "dup"    -- Duplicate the top value on the stack
  show OpSwap       = "swap"   -- Swap the top two values on the stack
  show OpPop        = "pop"    -- Pop the top value from the stack

  -- List Operations
  show OpCons       = "cons"   -- Cons operator (prepend an item to a list)
  show OpAppend     = "append" -- Append two lists together
  show OpHead       = "head"   -- Get the head of a list
  show OpTail       = "tail"   -- Get the tail of a list
  show OpEmpty      = "empty"  -- Check if a list is empty (return boolean)
  show OpLength     = "length" -- Get the length of a list
  show OpEach       = "each"   -- Apply a function (quotation) to each item in a list
  show OpMap        = "map"    -- Apply a function (quotation) to each item in a list and return a new list
  show OpFoldl      = "foldl"  -- Fold left operator (list -> accumulator -> final value)

  -- I/O and Parsing
  show OpPrint      = "print"  -- Print the top value from the stack
  show OpRead       = "read"   -- Read input

  -- String parsing
  show OpParseInt   = "parseInt" -- Parse an integer from input
  show OpParseFloat = "parseFloat" -- Parse a floating point number from input
  show OpWords      = "words"  -- Convert a string into a list of words (split by space)

-- Custom Show instance for 'Token', for printing tokens.
instance Show Token where
  show (TokVal v) = show v
  show (TokOp op) = show op

-- | Checks whether two 'Value's have the same constructor (i.e., are of the same type),
-- ignoring their inner contents.
--
-- This is useful for determining if two values are type-compatible without comparing
-- their actual contents.
--
sameConstructor :: Value -> Value -> Bool
sameConstructor (VInt _)     (VInt _)     = True
sameConstructor (VFloat _)   (VFloat _)   = True
sameConstructor (VBool _)    (VBool _)    = True
sameConstructor (VString _)  (VString _)  = True
sameConstructor (VList _)    (VList _)    = True
sameConstructor _            _            = False