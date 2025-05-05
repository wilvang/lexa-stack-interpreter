-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import qualified Data.Map as M
import Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul, safeDiv, safeIntDiv)
import Interpreter.Builtins.Comparison (safeEQ, safeLT, safeGT)
import Interpreter.Builtins.Logic (safeOr, safeAnd, safeNot)
import Interpreter.Types (Token(..), Op(..), Value(..))
import Interpreter.Error (ProgramError(..), BError(..))
import Interpreter.State ( State(..), initialStateWithDict)
import Interpreter.Parser


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
interpret :: String -> M.Map String Value -> Either BError State
interpret input dict = case parseTokens input of
      Left err     -> Left err
      Right tokens -> process (initialStateWithDict dict tokens)
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: State -> Either BError State
    process st@State{ program = [] } = Right st  -- No more tokens to process
    process st@State{ program = token:_ } =
      case step token st of
        Left err       -> Left err  -- If step produces an error, propagate it
        Right newState -> process newState  -- Continue processing with the new state

-- | The 'step' function is responsible for processing a single token and updating the stack.
--
-- == Examples:
--
-- $setup
-- >>> import Interpreter.State ( initialStateWithStack )
--
-- -- >>> step (TokVal (VInt 5)) (initialStateWithStack [])
-- Right [5]
--
-- >>> step (TokVal (VBool True)) (initialStateWithStack [VInt 1])
-- Right [True,1]
--
-- >>> step (TokOp OpAdd) (initialStateWithStack [VInt 2, VInt 3])
-- Right [5]
--
-- >>> step (TokOp OpSub) (initialStateWithStack [VInt 2, VInt 3])
-- Right [1]
--
-- >>> step (TokOp OpMul) (initialStateWithStack [VInt 2, VInt 3])
-- Right [6]
--
-- >>> step (TokOp OpDiv) (initialStateWithStack [VInt 10, VInt 2])
-- Right [0.2]
--
-- >>> step (TokOp OpAdd) (initialStateWithStack [VInt 2])  -- not enough operands
-- Right [2]
step :: Token -> State -> Either BError State
step (TokVal val) = Right . \st -> st { stack = val : stack st }
step (TokOp op) = case op of
  -- Arithmetic operations (binary)
  OpAdd    -> applyBinaryOp safeAdd
  OpSub    -> applyBinaryOp safeSub 
  OpMul    -> applyBinaryOp safeMul 
  OpDiv    -> applyBinaryOp safeDiv 
  OpIDiv   -> applyBinaryOp safeIntDiv 

  -- Comparison operations
  OpLT     -> applyBinaryOp safeLT 
  OpGT     -> applyBinaryOp safeGT 
  OpEQ     -> applyBinaryOp safeEQ

  -- Logical operations
  OpAnd    -> applyBinaryOp safeAnd
  OpOr     -> applyBinaryOp safeOr
  OpNot    -> applyUnaryOp safeNot
  _        -> unknownOp

{-
  -- Stack Manipulation
  OpDup    -> applyDupOp
  OpSwap   -> applySwapOp
  OpPop    -> applyPopOp

  -- Control flow or function application
  OpExec   -> applyExecOp    -- Execute the quotation block
  OpIf     -> applyIfOp      -- Handle the "if" operation
  OpTimes  -> applyTimesOp   -- Handle the "times" operation
  OpLoop   -> applyLoopOp    -- Handle the "loop" operation
  OpAssign -> applyAssignOp
  OpFun    -> applyFunOp

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

-- | Discards the state and returns a `ProgramError`.
-- This function is used when an invalid or unsupported operation is encountered.
-- It consumes the current stack and returns a `Left` value with a specific error.
--
-- == Examples:
--
-- >>> unknownOp (initialStateWithStack [])
-- Left (ProgramError UnknownSymbol)
--
-- >>> unknownOp (initialStateWithStack [VInt 5])
-- Left (ProgramError UnknownSymbol)
--
-- This function is typically used when an unrecognized operation is encountered,
-- and it explicitly discards the stack to avoid continuing with invalid state.
unknownOp :: State -> Either BError State
unknownOp _ = Left . ProgramError $ UnknownSymbol

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
-- >>> applyBinaryOp safeAdd (initialStateWithStack [VInt 1, VInt 2])
-- Right [3]
--
-- >>> applyBinaryOp safeMul (initialStateWithStack [VInt 3, VInt 4])
-- Right [12]
--
-- >>> applyBinaryOp safeDiv (initialStateWithStack [VFloat 2.0, VFloat 6.0])
-- Right [3.0]
--
-- >>> applyBinaryOp safeSub (initialStateWithStack [VInt 3, VInt 5])
-- Right [2]
--
-- >>> applyBinaryOp safeAdd (initialStateWithStack [])
-- Right []
--
-- >>> applyBinaryOp safeDiv (initialStateWithStack [VFloat 0.0, VFloat 6.0])
-- Left (ProgramError DivisionByZero)
applyBinaryOp :: (Value -> Value -> Either BError Value) -> State -> Either BError State
applyBinaryOp _ st@State{ stack = [] } = Right $ nextToken st
applyBinaryOp _ st@State{ stack = [_] } = Right $ nextToken st
applyBinaryOp op st@State{ stack = x:y:_ } = 
  case y `op` x of
    Right val  -> Right . nextToken $ pushValue val (popValue (popValue st))
    Left err   -> Left err

-- | Applies a unary operation to the top element of the stack.
-- This function takes a unary operator and a stack, applies the operator to the 
-- top element of the stack, and returns either a new stack with the result or an error.
--
-- It handles two cases:
-- 1. If the stack is empty, it returns the empty stack wrapped in 'Right', unchanged.
-- 2. If the stack contains at least one element, it applies the operator to the top element.
--  
-- == Examples:
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [])
-- Right []
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [VBool True])
-- Right [False]
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [VInt 42])
-- Right [-42]
--
applyUnaryOp :: (Value -> Either BError Value) -> State -> Either BError State
applyUnaryOp _ st@State{ stack = [] } = Right $ nextToken st
applyUnaryOp op st@State{ stack = x:_ } = 
  case op x of
    Right val  -> Right . nextToken $ pushValue val (popValue st)
    Left err   -> Left err

-- | Pushes a value onto the top of the stack.
--
-- === Examples:
--
-- >>> pushValue (VInt 42) (initialStateWithStack [])
-- [42]
--
-- >>> pushValue (VBool True) (initialStateWithStack [VInt 1])
-- [True,1]
--
pushValue :: Value -> State -> State
pushValue val st = st { stack = val : stack st }

-- | Removes the top value from the stack.
-- If the stack is empty, it is returned unchanged.
--
-- === Examples:
--
-- >>> popValue (initialStateWithStack [VInt 1, VInt 2])
-- [2]
--
-- >>> popValue (initialStateWithStack [VInt 42])
-- []
--
-- >>> popValue (initialStateWithStack [])
-- []
--
popValue :: State -> State
popValue st@State{ stack = [] } = st
popValue st@State{ stack = [_] } = st { stack = [] }
popValue st@State{ stack = _:rest } = st { stack = rest }

-- | Advances the program by removing the next token from the instruction list.
-- If the program is empty, it is returned unchanged.
--
nextToken :: State -> State
nextToken st@State{ program = [] } = st
nextToken st@State{ program = [_] } = st { program = [] }
nextToken st@State{ program = _:xs } = st { program = xs }
