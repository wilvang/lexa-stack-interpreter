-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import qualified Data.Map as M
import Control.Monad ( (>=>), foldM )
import Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul, safeDiv, safeIntDiv)
import Interpreter.Builtins.Comparison (safeEQ, safeLT, safeGT)
import Interpreter.Builtins.Logic (safeOr, safeAnd, safeNot)
import Interpreter.Builtins.List (safeHead, safeTail, safeEmpty, safeLength, safeCons, safeAppend)
import Interpreter.Types (Token(..), Op(..), Value(..))
import Interpreter.Error (ProgramError(..), BError(..))
import Interpreter.State ( State(..), lookupValues, initialStateWithDict)
import Interpreter.Parser


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
--
-- == Examples:
--
-- -- >>> interpret "1 2 +" M.empty
-- Right State{[3], []}
--
-- >>> interpret "3.0 1.5 -" M.empty
-- Right State{[1.5], []}
--
-- >>> interpret "10 2 /" M.empty
-- Right State{[5.0], []}
--
-- >>> interpret "1 2 unknown_val" M.empty
-- Right State{[unknown_val,2,1], []}
--
-- >>> interpret "True if { 10 } { }" M.empty
-- Right State{[10], []}
--
-- >>> interpret "10 9 - 0.0 >" M.empty
-- Right State{[True], []}
--
-- >>> interpret "{ 1 2" M.empty
-- Left (ParserError (IncompleteQuotation "{ 1 2"))
--
-- >>> interpret "10 0 /" M.empty
-- Left (ProgramError DivisionByZero)
-- >>> interpret "true false +" M.empty
-- Left (ProgramError ExpectedBoolOrNumber)
--
interpret :: String -> M.Map String Value -> Either BError State
interpret input dict = case parseTokens input of
      Left err     -> Left err
      Right tokens -> process (initialStateWithDict dict tokens)
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: State -> Either BError State
    process st@State{ program = [] } = Right st  -- No more tokens to process
    process st@State{ program = token:_ } =
      case step token (nextToken st) of
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
step (TokVal var@(VSymbol _)) = \st -> case lookupValues st [var] of
  [val@(VQuotation _)] -> pushValue val st >>= execValue
  _                    -> pushValue var st
step (TokVal val) = pushValue val
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
  OpDup    -> dupValue
  OpSwap   -> swapValue
  OpPop    -> popValue

  -- Control flow or function application
  OpExec   -> execValue
  OpIf     -> applyIfOp
  OpTimes  -> applyTimesOp
  OpLoop   -> applyLoopOp
  OpAssign -> assignValue
  OpFun    -> assignFunc

  -- List operations
  OpCons   -> applyBinaryOp safeCons   
  OpAppend -> applyBinaryOp safeAppend 
  OpHead   -> applyUnaryOp safeHead   
  OpTail   -> applyUnaryOp safeTail  
  OpEmpty  -> applyUnaryOp safeEmpty  
  OpLength -> applyUnaryOp safeLength
  --OpEach   -> applyEachOp     
  --OpMap    -> applyMapOp   
  --OpFoldl  -> applyFoldlOp   
  _        -> unknownOp

{-
  -- I/O operations
  OpPrint  -> applyPrintOp   -- Print the top value from the stack
  OpRead   -> applyReadOp    -- Read input (e.g., from user or file)
  OpParseInt -> applyParseIntOp  -- Parse an integer from input
  OpParseFloat -> applyParseFloatOp  -- Parse a float from input
  OpWords  -> applyWordsOp   -- Split a string into a list of words
 
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
applyBinaryOp _ st@State{ stack = [] } = Right st
applyBinaryOp _ st@State{ stack = [_] } = Right st
applyBinaryOp op st@State{ stack = x:y:_ } = 
  case lookupValues st [x, y] of
    [a, b] -> b `op` a >>= \val ->  -- Apply the binary operation to the top of the stack
      popValue >=> popValue >=> pushValue val >=> return $ st
    _      -> Left $ ProgramError ExpectedVariable
    

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
applyUnaryOp _ st@State{ stack = [] } = Right st
applyUnaryOp op st@State{ stack = x:_ } = 
  case lookupValues st [x] of
    [a] -> op a >>= \val ->
      popValue >=> pushValue val >=> return $ st
    _   -> Left $ ProgramError ExpectedVariable

-- | Evaluates an `if` operation by executing one of two quotations based on a boolean condition.
--
-- The top of the stack must be a 'VBool', and the next two program tokens must be quotations.
-- The first quotation is used if the boolean is True, the second if False.
--
-- == Examples:
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[1], []}
--
-- >>> applyIfOp (initialStateWithStack [VBool False] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[2], []}
--
-- >>> applyIfOp (initialStateWithStack [VInt 3] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Left (ProgramError ExpectedBool)
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VInt 2)])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> applyIfOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [])])
-- Left (ProgramError ExpectedVariable)
--
applyIfOp :: State -> Either BError State
applyIfOp st@State{ stack = x : rest, program = TokVal q1 : TokVal q2 : _ } =
  case lookupValues st [x, q1, q2] of
    [VBool cond, VQuotation path1, VQuotation path2] -> 
      let chosen = if cond then VQuotation path1 else VQuotation path2
      in execValue . nextToken $ nextToken st { stack = chosen : rest }
    [_, VQuotation _, VQuotation _] -> Left $ ProgramError ExpectedBool
    _                               -> Left $ ProgramError ExpectedQuotation
applyIfOp State{ stack = [] }  = Left $ ProgramError StackEmpty
applyIfOp State{ program = _ } = Left $ ProgramError ExpectedVariable

-- | Evaluates a `times` operation by executing a quotation `n` times.
--
-- The top of the stack must be a 'VInt', and the next token in the program must be a quotation.
-- The quotation is duplicated 'n' times (flattened) and prepended to the remaining program for execution.
--
-- == Examples:
--
-- >>> applyTimesOp (initialStateWithStack [VInt 3] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Right State{[], [1,1,1]}
--
-- >>> applyTimesOp (initialStateWithStack [VInt 0] [TokVal (VQuotation [TokVal (VInt 42)])])
-- Right State{[], []}
--
-- >>> applyTimesOp (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Left (ProgramError ExpectedEnumerable)
--
-- >>> applyTimesOp (initialStateWithStack [VInt 2] [TokVal (VInt 1)])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> applyTimesOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
-- >>> applyTimesOp (initialStateWithStack [VInt 2] [])
-- Left (ProgramError ExpectedQuotation)
--
applyTimesOp :: State -> Either BError State
applyTimesOp st@State{ stack = num : _, program = TokVal block : rest } = 
  case lookupValues st [num, block] of
    [VInt n, VQuotation quote] -> popValue st { program = concat (replicate n quote) ++ rest }
    [VInt _, _]              -> Left $ ProgramError ExpectedQuotation
    _                        -> Left $ ProgramError ExpectedEnumerable
applyTimesOp State{ stack = [] }  = Left $ ProgramError StackEmpty
applyTimesOp State{ program = _ } = Left $ ProgramError ExpectedQuotation

-- | Executes a loop using a break condition and a block of code.
--   
-- The loop takes two quotations from the program:
--
-- 1. A /break/ quotation that should leave a 'VBool' on the stack when executed.
-- 2. A /block/ quotation that will be executed repeatedly as long as the break condition evaluates to 'False'.
--
-- The break condition is checked before each iteration. If it evaluates to 'True', the loop terminates.
-- If it evaluates to 'False', the block is executed, and the loop repeats.
--
-- Both quotations are consumed from the program during the first iteration.
--
-- == Examples:
--
-- >>> applyLoopOp (initialStateWithStack [] [TokVal (VQuotation [TokVal (VBool True)]), TokVal (VQuotation [TokOp OpAdd])])
-- Right State{[], []}
--
-- >>> let break = TokVal (VQuotation [TokOp OpDup, TokVal (VInt 4), TokOp OpGT])
-- >>> let block = TokVal (VQuotation [TokOp OpDup, TokVal (VInt 1), TokOp OpAdd])
-- >>> applyLoopOp (initialStateWithStack [VInt 1] [break, block])
-- Right State{[5,4,3,2,1], []}
--
applyLoopOp :: State -> Either BError State
applyLoopOp st@State{ program = TokVal brk@(VQuotation _) : TokVal block@(VQuotation _) : _ } =
  pushValue brk st >>= execValue >>= \st' ->
    case stack st' of
      (VBool True:_)  -> popValue . nextToken $ nextToken st'
      (VBool False:_) -> popValue st' >>= pushValue block >>= execValue >>= applyLoopOp
      _               -> Left $ ProgramError ExpectedBool
applyLoopOp State{ program = _ } = Left $ ProgramError ExpectedVariable

-- | Assigns a value to a symbol in the state dictionary. The first operation in the program must be `OpAssign`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a value.
-- The symbol is associated with the value in the dictionary.
--
assign :: (Value -> Maybe Value) -> ProgramError -> State -> Either BError State
assign extract expectedError st@State{ stack = VSymbol x:_, program = TokVal val : _ } =
  maybe (Left $ ProgramError expectedError) 
        (popValue . nextToken . updateDictionary x) 
        (extract val)
  where
    updateDictionary key var = st { dictionary = M.insert key var (dictionary st) }
assign _ _ State{ stack = _ } = Left $ ProgramError ExpectedSymbol

-- | Assigns a value to a symbol in the state dictionary. The first operation in the program must be `OpAssign`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a value.
-- The symbol is associated with the value in the dictionary.
--
-- == Examples:
--
-- >>> assignValue (initialStateWithStack [VSymbol "x"] [TokVal (VInt 42)])
-- Right State{[], []}
--
-- >>> assignValue (initialStateWithStack [VSymbol "y"] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Right State{[], []}
--
-- >>> assignValue (initialStateWithStack [VInt 42] [TokVal (VInt 100)])
-- Left (ProgramError ExpectedSymbol)
--
-- >>> assignValue (initialStateWithStack [VSymbol "z"] [TokVal (VSymbol "b")])
-- Left (ProgramError ExpectedVariable)
--
assignValue :: State -> Either BError State
assignValue = assign extractValue ExpectedVariable
  where
    extractValue (VSymbol _) = Nothing
    extractValue v = Just v

-- | Assigns a function (quotation) to a symbol in the state dictionary. The first operation in the program must be `OpFun`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a 'VQuotation'.
-- The symbol is associated with the quotation in the dictionary.
--
-- == Examples:
--
-- >>> assignFunc (initialStateWithStack [VSymbol "func"] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Right State{[], []}
--
-- >>> assignFunc (initialStateWithStack [VSymbol "action"] [TokVal (VInt 42)])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> assignFunc (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 100)])])
-- Left (ProgramError ExpectedSymbol)
--
-- >>> assignFunc (initialStateWithStack [VSymbol "foo"] [TokVal (VInt 2)])
-- Left (ProgramError ExpectedQuotation)
--
assignFunc :: State -> Either BError State
assignFunc = assign extractQuotation ExpectedQuotation
  where
    extractQuotation (VQuotation q) = Just (VQuotation q)
    extractQuotation _ = Nothing

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
-- >>> execValue (initialStateWithStack [VQuotation [TokOp OpAdd], VInt 1, VInt 2] [])
-- Right State{[3], []}
--
-- >>> execValue (initialStateWithStack [VQuotation [TokOp OpDup, TokVal (VInt 4), TokOp OpGT], VInt 1] [])
-- Right State{[False,1], []}
--
-- >>> execValue (initialStateWithStack [VInt 42] [])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> execValue (initialStateWithStack [] [])
-- Left (ProgramError ExpectedQuotation)
--
execValue :: State -> Either BError State
execValue st@State{ stack = VQuotation quote : _ } = 
   -- Execute the quotation
  popValue st >>= \st' -> 
    foldM (flip step) st' quote 
execValue State{ stack = _ } = Left $ ProgramError ExpectedQuotation

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