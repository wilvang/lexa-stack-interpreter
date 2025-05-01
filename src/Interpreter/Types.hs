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

>>> let vInt = VInt 10
>>> show vInt
"10"

>>> let vFloat = VFloat 3.14
>>> show vFloat
"3.14"

>>> let vBool = VBool True
>>> show vBool
"True"

>>> let vString = VString "Hello"
>>> show vString
"\"Hello\""

>>> let vList = VList [VInt 1, VInt 2, VInt 3]
>>> show vList
"[ 1 2 3 ]"

>>> let vQuotation = VQuotation [TokVal (VInt 5), TokOp (OpAdd)]
>>> show vQuotation
"{ 5 + }"

>>> let symbol = VSymbol "myVar"
>>> show symbol
"myVar"

Operations:
-----------
1. The `Op` type defines operations like addition, multiplication, comparison, etc.

>>> show OpAdd
"+"

>>> show OpSub
"-"

>>> show OpMul
"*"

>>> show OpDiv
"/"

>>> show OpIDiv
"div"

>>> show OpLT
"<"

>>> show OpGT
">"

>>> show OpEQ
"=="

>>> show OpAnd
"&&"

>>> show OpOr
"||"

>>> show OpNot
"not"

>>> show OpExec
"exec"

>>> show OpIf
"if"

>>> show OpTimes
"times"

>>> show OpAssign
":="

>>> show OpFun
"fun"

Token Type:
------------
1. The `Token` type wraps either a value or an operation.

>>> let tVal = TokVal (VInt 42)
>>> show tVal
"42"

>>> let tOp = TokOp OpAdd
>>> show tOp
"+"
-}

module Interpreter.Types where

-- | Core data types

-- 'Value' represents all possible values that can appear on the stack.
data Value
  = VInt Int          -- Integer value
  | VFloat Double     -- Floating-point value
  | VBool Bool        -- Boolean value (True or False)
  | VString String    -- A literal string value (e.g., "Hello")
  | VList [Value]     -- A list of values (e.g., [1 2 3])
  | VQuotation [Token] -- A code block (quotation), a list of tokens to be executed
  | VSymbol String    -- A symbol, typically used for variable or function names (e.g., `name` or `inc`)
  deriving (Eq)       -- Eq allows for comparison (e.g., checking equality between two values)

-- | Operations (Op) represent the various commands and operators in the language.
data Op
  = OpAdd       -- Addition operator (x + y)
  | OpSub       -- Subtraction operator (x - y)
  | OpMul       -- Multiplication operator (x * y)
  | OpDiv       -- Floating point division operator (x / y)
  | OpIDiv      -- Integer division operator (x div y)
  | OpLT        -- Less-than comparison operator (x < y)
  | OpGT        -- Greater-than comparison operator (x > y)
  | OpEQ        -- Equality comparison operator (x == y)
  | OpAnd       -- Logical AND operator (x && y)
  | OpOr        -- Logical OR operator (x || y)
  | OpNot       -- Logical NOT operator (not x)
  | OpExec      -- Execute a quotation (block of code)
  | OpIf        -- If statement (x if condition)
  | OpTimes     -- Repeat block n times
  | OpLoop      -- Loop trough a block, based on a condition
  | OpAssign    -- Assignment operator (e.g., name := value)
  | OpFun       -- Function definition (e.g., fun inc { 1 + })
  deriving (Eq)  -- Eq allows for comparison (e.g., checking equality between two operations)

-- | Tokens are either values or operations (commands).
-- A 'Token' represents an individual element of the language (either a value or an operation).
data Token
  = TokVal Value   -- A token holding a value (e.g., 10, "hello")
  | TokOp Op       -- A token holding an operation (e.g., +, :=, if)
  deriving (Eq) -- Eq for comparison between two tokens

-- Custom Show instance for 'Value', for printing values.
instance Show Value where
  show (VInt n)       = show n               -- Display an integer as is
  show (VFloat f)     = show f               -- Display a floating-point number as is
  show (VBool b)      = show b               -- Display a boolean as is (True or False)
  show (VString s)    = show s               -- Display a string as is (e.g., "hello")
  show (VList xs)     = "[ " ++ unwords (show <$> xs) ++ " ]"  -- Display list of values, separated by space
  show (VQuotation q) = "{ " ++ unwords (show <$> q) ++ " }"   -- Display a quotation (code block) as space-separated tokens
  show (VSymbol s)    = s                     -- Display the symbol as is (e.g., variable name 'name')

-- Custom Show instance for 'Op', for printing operations.
instance Show Op where
  show OpAdd    = "+"      -- Display the addition operation as "+"
  show OpSub    = "-"      -- Display the subtraction operation as "-"
  show OpMul    = "*"      -- Display the multiplication operation as "*"
  show OpDiv    = "/"      -- Display the division operation as "/"
  show OpIDiv   = "div"    -- Display the integer division operation as "div"
  show OpLT     = "<"      -- Display the less-than operator as "<"
  show OpGT     = ">"      -- Display the greater-than operator as ">"
  show OpEQ     = "=="     -- Display the equality operator as "=="
  show OpAnd    = "&&"     -- Display the logical AND operator as "&&"
  show OpOr     = "||"     -- Display the logical OR operator as "||"
  show OpNot    = "not"    -- Display the logical NOT operator as "not"
  show OpExec   = "exec"   -- Display the execute operation as "exec"
  show OpIf     = "if"     -- Display the if operation as "if"
  show OpTimes  = "times"  -- Display the times operation as "times"
  show OpLoop   = "loop"   -- Display the loop operation as "loop"
  show OpAssign = ":="     -- Display the assignment operation as ":="
  show OpFun    = "fun"    -- Display the function definition as "fun"

-- Custom Show instance for 'Token', for printing tokens.
instance Show Token where
    show (TokVal v) = show v
    show (TokOp op) = show op
