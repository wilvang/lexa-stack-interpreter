-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import qualified Data.Map as M
import Control.Monad ( (>=>) )
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
-- -- >>> step (TokVal (VInt 5)) (initialStateWithStack [] [])
-- Right State{[5], []}
--
-- >>> step (TokVal (VBool True)) (initialStateWithStack [VInt 1] [])
-- Right State{[True,1], []}
--
-- >>> step (TokOp OpAdd) (initialStateWithStack [VInt 2, VInt 3] [])
-- Right State{[5], []}
--
-- >>> step (TokOp OpSub) (initialStateWithStack [VInt 2, VInt 3] [])
-- Right State{[1], []}
--
-- >>> step (TokOp OpMul) (initialStateWithStack [VInt 2, VInt 3] [])
-- Right State{[6], []}
--
-- >>> step (TokOp OpDiv) (initialStateWithStack [VInt 10, VInt 2] [])
-- Right State{[0.2], []}
--
-- >>> step (TokOp OpAdd) (initialStateWithStack [VInt 2] [])  -- not enough operands
-- Right State{[2], []}
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

  -- Stack Manipulation
  OpDup    -> dupValue . nextToken
  OpSwap   -> swapValue . nextToken
  OpPop    -> popValue . nextToken

  -- Control flow or function application
  OpExec   -> execValue    -- Execute the quotation block
  OpIf     -> applyIfOp      -- Handle the "if" operation
  OpTimes  -> applyTimesOp   -- Handle the "times" operation
  _        -> unknownOp

  {-
  OpLoop   -> applyNon    -- Handle the "loop" operation
  OpAssign -> applyBinaryOp
  OpFun    -> applyBinaryOp

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
-- >>> unknownOp (initialStateWithStack [] [])
-- Left (ProgramError UnknownSymbol)
--
-- >>> unknownOp (initialStateWithStack [VInt 5] [])
-- Left (ProgramError UnknownSymbol)
--
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
-- >>> applyBinaryOp safeAdd (initialStateWithStack [VInt 1, VInt 2] [])
-- Right State{[3], []}
--
-- >>> applyBinaryOp safeMul (initialStateWithStack [VInt 3, VInt 4] [])
-- Right State{[12], []}
--
-- >>> applyBinaryOp safeDiv (initialStateWithStack [VFloat 2.0, VFloat 6.0] [])
-- Right State{[3.0], []}
--
-- >>> applyBinaryOp safeSub (initialStateWithStack [VInt 3, VInt 5] [])
-- Right State{[2], []}
--
-- >>> applyBinaryOp safeAdd (initialStateWithStack [] [])
-- Right State{[], []}
--
-- >>> applyBinaryOp safeDiv (initialStateWithStack [VFloat 0.0, VFloat 6.0] [])
-- Left (ProgramError DivisionByZero)
applyBinaryOp :: (Value -> Value -> Either BError Value) -> State -> Either BError State
applyBinaryOp _ st@State{ stack = [] } = Right $ nextToken st
applyBinaryOp _ st@State{ stack = [_] } = Right $ nextToken st
applyBinaryOp op st@State{ stack = x:y:_ } = 
  y `op` x >>= \val ->  -- Apply the binary operation to the top of the stack
  popValue >=>          -- Pop the top value from the stack
  popValue >=>          -- Pop the second value from the stack
  pushValue val >=>     -- Push the result back onto the stack
  return . nextToken
  $ st

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
-- >>> applyUnaryOp safeNot (initialStateWithStack [VBool True] [])
-- Right State{[False], []}
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [VBool False] [])
-- Right State{[True], []}
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [VInt 1] [])
-- Right State{[-1], []}
--
-- >>> applyUnaryOp safeNot (initialStateWithStack [] [])
-- Right State{[], []}
-- >>> applyUnaryOp safeNot (initialStateWithStack [VString "ops"] [])
-- Left (ProgramError ExpectedBoolOrNumber)
--
applyUnaryOp :: (Value -> Either BError Value) -> State -> Either BError State
applyUnaryOp _ st@State{ stack = [] } = Right $ nextToken st
applyUnaryOp op st@State{ stack = x:_ } = 
  op x >>= \val ->    -- Apply the unary operation to the top of the stack
  popValue >=>        -- Pop the top value from the stack
  pushValue val >=>   -- Push the result back onto the stack
  return . nextToken
  $ st

-- | Evaluates an `if` operation by executing one of two quotations based on a boolean condition.
--
-- The top of the stack must be a 'VBool', and the next two program tokens must be quotations.
-- The first quotation is used if the boolean is True, the second if False.
--
-- == Examples:
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokOp OpIf, TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[], [1]}
--
-- >>> applyIfOp (initialStateWithStack [VBool False] [TokOp OpIf, TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[], [2]}
--
-- >>> applyIfOp (initialStateWithStack [VInt 3] [TokOp OpIf, TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Left (ProgramError ExpectedBool)
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokOp OpIf, TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VInt 2)])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> applyIfOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [])])
-- Left (ProgramError ExpectedVariable)
--
applyIfOp :: State -> Either BError State
applyIfOp State{ stack = [], program = _ } = Left $ ProgramError StackEmpty
applyIfOp State{ stack = _, program = [] } = Left $ ProgramError ExpectedVariable
applyIfOp State{ stack = _, program = [_] } = Left $ ProgramError ExpectedVariable
applyIfOp st = case stack st of
  (VBool cond : rest) -> case program st of
    (_ : TokVal(VQuotation q1) : TokVal(VQuotation q2) : _) ->
      execValue . nextToken $ nextToken st { stack = chosen : rest }
      where
        chosen = if cond then VQuotation q1 else VQuotation q2
    _ -> Left $ ProgramError ExpectedQuotation
  _ -> Left $ ProgramError ExpectedBool
  

-- | Evaluates a `times` operation by executing a quotation `n` times.
--
-- The top of the stack must be a 'VInt', and the next token in the program must be a quotation.
-- The quotation is duplicated 'n' times (flattened) and prepended to the remaining program for execution.
--
-- == Examples:
--
-- >>> applyTimesOp (initialStateWithStack [VInt 3] [TokOp OpTimes, TokVal (VQuotation [TokVal (VInt 1)])])
-- Right State{[], [1,1,1]}
--
-- >>> applyTimesOp (initialStateWithStack [VInt 0] [TokOp OpTimes, TokVal (VQuotation [TokVal (VInt 42)])])
-- Right State{[], []}
--
-- >>> applyTimesOp (initialStateWithStack [VBool True] [TokOp OpTimes, TokVal (VQuotation [TokVal (VInt 1)])])
-- Left (ProgramError ExpectedEnumerable)
--
-- >>> applyTimesOp (initialStateWithStack [VInt 2] [TokOp OpTimes, TokVal (VInt 1)])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> applyTimesOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
-- >>> applyTimesOp (initialStateWithStack [VInt 2] [])
-- Left (ProgramError ExpectedQuotation)
--
applyTimesOp :: State -> Either BError State
applyTimesOp State{ stack = [] } = Left $ ProgramError StackEmpty
applyTimesOp st@State{ stack = x:_, program = _ : TokVal(VQuotation q1) : rest } = 
  case x of
    (VInt n)   -> popValue st { program = concat (replicate n q1) ++ rest }
    _          -> Left $ ProgramError ExpectedEnumerable
applyTimesOp State{ program = _ } = Left $ ProgramError ExpectedQuotation

-- | Executes a quotation at the top of the stack.
--
-- This function checks if the top value on the stack is a 'VQuotation',
-- and if the next token in the program is an 'OpExec'. If so, it replaces
-- the program with the contents of the quotation followed by the rest
-- of the original program (after the OpExec), and removes the quotation
-- from the stack.
--
-- == Behavior
--
-- * If the stack is empty, returns 'StackEmpty' error.
-- * If the top of the stack is not a quotation, returns 'ExpectedQuotation' error.
-- * If the program does not contain an 'OpExec', returns 'ExpectedQuotation' error.
-- * Otherwise, executes the quotation by updating the program.
--
-- == Examples
--
-- >>> execValue (initialStateWithStack [VQuotation [TokVal (VInt 1), TokVal (VInt 2)]] [TokOp OpExec, TokOp OpExec])
-- Right State{[], [1,2,exec]}
--
-- >>> execValue (initialStateWithStack [VQuotation [TokVal (VInt 1), TokVal (VInt 2)]] [TokOp OpExec])
-- Right State{[], [1,2]}
--
-- >>> execValue (initialStateWithStack [VInt 42] [TokOp OpExec])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> execValue (initialStateWithStack [] [TokOp OpExec])
-- Left (ProgramError StackEmpty)
--
execValue :: State -> Either BError State
execValue State{ stack = [], program = _ } = Left $ ProgramError StackEmpty
execValue st@State{ stack = x:_, program = p } = case (x, p) of
  (VQuotation q, _:rest)  -> popValue st { program = q ++ rest}
  _              -> Left $ ProgramError ExpectedQuotation

-- | Pushes a value onto the top of the stack.
--
-- === Examples:
--
-- >>> pushValue (VInt 42) (initialStateWithStack [] [])
-- Right State{[42], []}
--
-- >>> pushValue (VBool True) (initialStateWithStack [VInt 1] [])
-- Right State{[True,1], []}
--
pushValue :: Value -> State -> Either BError State
pushValue val st = Right st { stack = val : stack st }

-- | Removes the top value from the stack.
-- If the stack is empty, it is returned unchanged.
--
-- === Examples:
--
-- >>> popValue (initialStateWithStack [VInt 1, VInt 2] [])
-- Right State{[2], []}
--
-- >>> popValue (initialStateWithStack [VInt 42] [])
-- Right State{[], []}
--
-- >>> popValue (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
popValue :: State -> Either BError State
popValue st = case st of
  State{ stack = [] } -> Left $ ProgramError StackEmpty
  State{ stack = [_] } -> Right st { stack = [] }
  State{ stack = _:rest } -> Right st { stack = rest }

-- | Duplicates the top value on the stack.
-- If the stack is empty, returns a 'StackEmpty' error.
--
-- === Examples:
--
-- >>> dupValue (initialStateWithStack [VInt 1] [])
-- Right State{[1,1], []}
--
-- >>> dupValue (initialStateWithStack [VBool True, VInt 5] [])
-- Right State{[True,True,5], []}
--
-- >>> dupValue (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
dupValue :: State -> Either BError State
dupValue st = case st of
  State{ stack = [] }     -> Left $ ProgramError StackEmpty
  State{ stack = x:rest } -> Right st { stack = x:x:rest }

-- | Swaps the top two values on the stack.
-- If the stack has fewer than two elements, returns an appropriate error.
--
-- === Examples:
--
-- >>> swapValue (initialStateWithStack [VInt 1, VInt 2] [])
-- Right State{[2,1], []}
--
-- >>> swapValue (initialStateWithStack [VBool False, VString "hi"] [])
-- Right State{["hi",False], []}
--
-- >>> swapValue (initialStateWithStack [VInt 1] [])
-- Left (ProgramError ExpectedVariable)
--
-- >>> swapValue (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
swapValue :: State -> Either BError State
swapValue st = case st of
  State{ stack = [] }       -> Left $ ProgramError StackEmpty
  State{ stack = [_] }      -> Left $ ProgramError ExpectedVariable
  State{ stack = x:y:rest } -> Right st { stack = y:x:rest }

-- | Advances the program by removing the next token from the instruction list.
-- If the program is empty, it is returned unchanged.
--
-- == Examples:
--
-- >>> nextToken (initialStateWithStack [] [TokOp OpSub, TokVal (VInt 5)])
-- State{[], [5]}
--
-- >>> nextToken (initialStateWithStack [] [TokOp  OpSub])
-- State{[], []}
--
-- >>> nextToken (initialStateWithStack [] [])
-- State{[], []}
--
nextToken :: State -> State
nextToken st@State{ program = [] } = st
nextToken st@State{ program = [_] } = st { program = [] }
nextToken st@State{ program = _:xs } = st { program = xs }