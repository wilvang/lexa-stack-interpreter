-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul, safeDiv, safeIntDiv)
import Interpreter.Types (Token(..), Op(..), Value(..))
import Interpreter.Error (ProgramError(..), BError(..))
import Interpreter.Parser

type Stack = [Value]


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
interpret :: String -> Either BError Stack
interpret input = case parseTokens input of
      Left err     -> Left err
      Right tokens -> process tokens []
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: [Token] -> Stack -> Either BError Stack
    process [] stack = Right stack  -- Base case: no more tokens, return the final stack
    process (token : tokens) stack = 
      case step token stack of
        Left err       -> Left err  -- If step produces an error, propagate it
        Right newStack -> process tokens newStack  -- Continue processing with the new stack

-- | The 'step' function is responsible for processing a single token and updating the stack.
step :: Token -> Stack -> Either BError Stack
step (TokVal val) = Right . (val :) -- Push the value onto the stack
step (TokOp op)  = case op of
  -- Arithmetic operations (binary)
  OpAdd    -> applyBinaryOp safeAdd
  OpSub    -> applyBinaryOp safeSub 
  OpMul    -> applyBinaryOp safeMul 
  OpDiv    -> applyBinaryOp safeDiv 
  OpIDiv   -> applyBinaryOp safeIntDiv 
  _        -> discardStack
{-
  -- Comparison operations
  OpLT     -> applyOpComparison (<) 
  OpGT     -> applyOpComparison (>) 
  OpEQ     -> applyOpComparison (==)

  -- Logical operations
  OpAnd    -> applyLogicalOp (&&)
  OpOr     -> applyLogicalOp (||)
  OpNot    -> applyNotOp

  -- Control flow or function application
  OpExec   -> applyExecOp    -- Execute the quotation block
  OpIf     -> applyIfOp      -- Handle the "if" operation
  OpTimes  -> applyTimesOp   -- Handle the "times" operation
  OpLoop   -> applyLoopOp    -- Handle the "loop" operation

  -- List operations
  OpCons   -> applyConsOp    -- Prepend an item to a list
  OpAppend -> applyAppendOp  -- Append two lists
  OpHead   -> applyHeadOp    -- Get the head of the list
  OpTail   -> applyTailOp    -- Get the tail of the list
  OpEmpty  -> applyEmptyOp   -- Check if the list is empty
  OpLength -> applyLengthOp  -- Get the length of the list
  OpEach   -> applyEachOp    -- Apply a function to each element in the list
  OpMap    -> applyMapOp     -- Map a function over a list

  -- I/O operations
  OpPrint  -> applyPrintOp   -- Print the top value from the stack
  OpRead   -> applyReadOp    -- Read input (e.g., from user or file)
  OpParseInt -> applyParseIntOp  -- Parse an integer from input
  OpParseFloat -> applyParseFloatOp  -- Parse a float from input
  OpWords  -> applyWordsOp   -- Split a string into a list of words

  -- Higher-order functions
  OpFoldl  -> applyFoldlOp   -- Apply a fold-left operation on a list
-}

-- | Discards the stack and returns a `ProgramError`.
-- This function is used when an invalid or unsupported operation is encountered.
-- It consumes the current stack and returns a `Left` value with a specific error.
--
-- == Examples:
--
-- >>> discardStack []
-- Left (ProgramError UnknownSymbol)
--
-- >>> discardStack [VInt 5]
-- Left (ProgramError UnknownSymbol)
--
-- This function is typically used when an unrecognized operation is encountered,
-- and it explicitly discards the stack to avoid continuing with invalid state.
discardStack :: Stack -> Either BError Stack
discardStack _ = Left . ProgramError $ UnknownSymbol

-- | Applies a binary operation to the top two elements of the stack.
-- This function takes an operator and a stack, applies the operator to the 
-- top two elements of the stack, and returns a new stack with the result.
-- If the operator results in an error, the function returns the error without modifying the stack.
--
-- It handles three cases:
-- 1. If the stack is empty, it returns the stack unchanged.
-- 2. If the stack contains only one element, it returns the stack unchanged.
-- 3. If the stack contains two or more elements, it applies the operator to the top two elements
--    and returns the result as the new top of the stack.
--
-- == Examples:
--
-- >>> applyBinaryOp safeAdd [VInt 1, VInt 2]
-- Right [3]
--
-- >>> applyBinaryOp safeMul [VInt 3, VInt 4]
-- Right [12]
--
-- >>> applyBinaryOp safeDiv [VFloat 2.0, VFloat 6.0]
-- Right [3.0]
--
-- >>> applyBinaryOp safeSub [VInt 3, VInt 5]
-- Right [2]
--
-- >>> applyBinaryOp safeAdd []
-- Right []
--
-- >>> applyBinaryOp safeDiv [VFloat 0.0, VFloat 6.0]
-- Left (ProgramError DivisionByZero)
applyBinaryOp :: (Value -> Value -> Either BError Value) -> Stack -> Either BError Stack
applyBinaryOp _ [] = Right []
applyBinaryOp _ s@[_] = Right s
applyBinaryOp op (x:y:rest) = 
  case y `op` x of
    Right z    -> Right (z : rest)
    Left err   -> Left err
