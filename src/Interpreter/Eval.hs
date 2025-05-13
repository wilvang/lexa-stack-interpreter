-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import qualified Data.Map as M
import Control.Monad ( (>=>), foldM )
import Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul, safeDiv, safeIntDiv)
import Interpreter.Builtins.Comparison (safeEQ, safeLT, safeGT)
import Interpreter.Builtins.Logic (safeOr, safeAnd, safeNot)
import Interpreter.Builtins.StringParsing (safeWords, safeIntParse, safeFloatParse)
import Interpreter.Builtins.List (safeHead, safeTail, safeEmpty, safeLength, safeCons, safeAppend)
import Interpreter.State ( State(..), Interrupt(..), lookupValue, setInterrupt)
import Interpreter.Types (Token(..), Op(..), Value(..))
import Interpreter.Error (ProgramError(..), BError(..))
import Interpreter.Parser


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
--
-- == Examples:
--
-- $setup
-- >>> import Interpreter.State ( initialStateWithStack )
--
-- >>> interpret "1 2 +" (initialStateWithStack [] [])
-- Right State{[3], []}
--
-- >>> interpret "3.0 1.5 -" (initialStateWithStack [] [])
-- Right State{[1.5], []}
--
-- >>> interpret "10 2 /" (initialStateWithStack [] [])
-- Right State{[5.0], []}
--
-- >>> interpret "1 2 unknown_val" (initialStateWithStack [] [])
-- Right State{[unknown_val,2,1], []}
--
-- >>> interpret "True if { 10 } { }" (initialStateWithStack [] [])
-- Right State{[10], []}
--
-- >>> interpret "5 times { 1 } [ ] 5 times { cons } 0 foldl { + }" (initialStateWithStack [] [])
-- Right State{[5], []}
--
-- >>> interpret "{ 1 2" (initialStateWithStack [] [])
-- Left (ParserError (IncompleteQuotation "{ 1 2"))
--
-- >>> interpret "10 0 /" (initialStateWithStack [] [])
-- Left (ProgramError DivisionByZero)
--
-- >>> interpret "true false +" (initialStateWithStack [] [])
-- Left (ProgramError ExpectedBoolOrNumber)
--
interpret :: String -> State -> Either BError State
interpret input state@State{ program = prog } = case parseTokens input of
      Left err     -> Left err
      Right tokens -> process (state { program = tokens ++ prog })
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: State -> Either BError State
    process st@State{ buffer = [_] } = Right st  -- I/O interrupt
    process st@State{ program = [] } = Right st  -- No more tokens to process
    process st@State{ program = token:_ } =
      case step token (nextToken st) of
        Left err       -> Left err  -- If step produces an error, propagate it
        Right newState -> process newState  -- Continue processing with the new state

-- | The 'step' function is responsible for processing a single token and updating the stack.
--
-- == Examples:
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
--
step :: Token -> State -> Either BError State
step (TokVal var@(VSymbol _)) = \st -> case lookupValue st var of
  VQuotation q -> interpret "" st { program = q ++ program st }
  _              -> pushValue var st
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
  OpEach   -> applyEachOp     
  OpMap    -> applyMapOp   
  OpFoldl  -> applyFoldlOp

  -- I/O operations
  OpPrint  -> applyPrintOp
  OpRead   -> Right . setInterrupt InputIO 

  -- String parsing
  OpParseInt   -> applyUnaryOp safeIntParse
  OpParseFloat -> applyUnaryOp safeFloatParse
  OpWords      -> applyUnaryOp safeWords  

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
--
applyBinaryOp :: (Value -> Value -> Either BError Value) -> State -> Either BError State
applyBinaryOp _ st@State{ stack = [] } = Right st
applyBinaryOp _ st@State{ stack = [_] } = Right st
applyBinaryOp op st@State{ stack = x:y:_ } = 
  case lookupValue st <$> [x, y] of
    [a, b] -> b `op` a >>= \val ->  -- Apply the binary operation to the top of the stack
      popValue >=> popValue >=> pushValue val >=> return $ st
    _      -> Left . ProgramError $ ExpectedVariable st
    

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
  case lookupValue st x of
    a -> op a >>= \val -> 
      popValue >=> pushValue val >=> return $ st

-- | Applies the `OpPrint` operation.
-- 
-- This operation extracts the top value from the stack, pops the value below it, 
-- and then sets the interrupt with the output of the first value.
--
-- The operation works as follows:
-- 1. It extracts the top value from the stack (using `extractTop`).
-- 2. It pops the next value from the stack (using `popValue`).
-- 3. It then sets the interrupt with an `OutputIO` using the first value, while leaving the modified stack as the result.
--
applyPrintOp :: State -> Either BError State
applyPrintOp st = extractTop st >>= \val -> 
    popValue st >>= \st' -> 
      Right $ setInterrupt (OutputIO val) st'

-- | Evaluates an `if` operation by executing one of two tokens based on a boolean condition.
--
-- The top of the stack must be a 'VBool', and the program need two tokens.
-- The first token is used if the boolean is True, the second if False.
--
-- == Examples:
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[1], []}
--
-- >>> applyIfOp (initialStateWithStack [VBool False] [TokOp OpAdd, TokVal (VQuotation [TokVal (VInt 2)])])
-- Right State{[2], []}
--
-- >>> applyIfOp (initialStateWithStack [VInt 3] [TokVal (VQuotation [TokVal (VInt 1)]), TokVal (VQuotation [TokVal (VInt 2)])])
-- Left (ProgramError ExpectedBool)
--
-- >>> applyIfOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
-- >>> applyIfOp (initialStateWithStack [VBool True] [TokVal (VQuotation [])])
-- Left (ProgramError ExpectedVariable)
--
applyIfOp :: State -> Either BError State
applyIfOp st@State{ stack = val : _, program = p1 : p2 : rest } =
  case lookupValue st val of
    VBool cond ->
      let chosen = if cond then p1 else p2
      in popValue st >>= handleChosen chosen rest
    _ -> Left $ ProgramError ExpectedBool
applyIfOp State{ stack = [] } = Left $ ProgramError StackEmpty
applyIfOp st = Left . ProgramError $ ExpectedVariable st

-- | Handles a single chosen 'Token' during evaluation.
-- If the token is a value, it either evaluates its quotation or pushes the resolved value onto the stack.
-- If the token is an operation, it applies the operation using 'step'.
-- After handling, the state's program is updated to continue with the remaining tokens.
--
handleChosen :: Token -> [Token] -> State -> Either BError State
handleChosen (TokVal v) rest st =
  case lookupValue st v of
    VQuotation body -> interpret "" st { program = body } >>= \st' -> pure st' { program = rest }
    val             -> pushValue val st >>= \st' -> pure st' { program = rest }
handleChosen op rest st =
  step op st >>= \st' -> pure st' { program = rest }

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
applyTimesOp st@State{ stack = num : _, program = block : rest } = 
  case (lookupValue st num, checkBlock st block) of
    (VInt n, VQuotation q) -> popValue st { program = concat (replicate (fromIntegral n) q) ++ rest }
    _                        -> Left $ ProgramError ExpectedEnumerable
applyTimesOp State{ stack = [] }  = Left $ ProgramError StackEmpty
applyTimesOp st@State{ program = _ } = Left . ProgramError $ ExpectedQuotation st


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
applyLoopOp st@State{ program = TokVal cond : TokVal block : _ } =
  case lookupValue st <$> [cond, block] of
    [a@(VQuotation _), b@(VQuotation _)] -> execLoop a b
    _ -> Left . ProgramError $ ExpectedQuotation st
    where 
      execLoop a b = pushValue a st >>= execValue >>= \st' ->
        case stack st' of
          (VBool True:_)  -> popValue . nextToken $ nextToken st'
          (VBool False:_) -> popValue st' >>= pushValue b >>= execValue >>= applyLoopOp
          _               -> Left $ ProgramError ExpectedBool
applyLoopOp st = Left . ProgramError $ ExpectedVariable st


-- | Executes a quotation on each element of a homogeneous list (`each` operation).
--
-- Pops a homogeneous list from the top of the stack and takes a quotation
-- from the next token in the program. The quotation is executed with each element
-- of the list as input, in sequence.
--
-- == Examples:
--
-- >>> let block = TokVal (VQuotation [TokVal (VInt 10), TokOp OpMul])
-- >>> applyEachOp (initialStateWithStack [VList [VInt 1, VInt 2]] [block])
-- Right State{[20,10], []}
--
-- >>> let block = TokVal (VQuotation [TokVal (VInt 1)])
-- >>> applyEachOp (initialStateWithStack [VList []] [block])
-- Right State{[], []}
--
-- >>> let block = TokVal (VQuotation [TokOp OpMul])
-- >>> applyEachOp (initialStateWithStack [VList [VInt 1, VBool True]] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Right State{[1,True,1,1], []}
--
-- >>> applyEachOp (initialStateWithStack [VList [VInt 1]] [TokVal (VInt 1)])
-- Left (ProgramError ExpectedList)
--
-- >>> applyEachOp (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
--
applyEachOp :: State -> Either BError State
applyEachOp st@State{ stack = list : _, program = block : _ } =
  case lookupValue st list of
    VList xs ->
      popValue st >>= \st' ->
        foldM
          (\s element -> pushValue element s 
          >>= pushValue (checkBlock st block)
          >>= execValue)
          (nextToken st')
          xs
    _ -> Left $ ProgramError ExpectedList
applyEachOp State{ stack = [] } = Left $ ProgramError StackEmpty
applyEachOp st = Left . ProgramError $ ExpectedQuotation st

-- | Applies a quotation to each element in a list and collects the results.
--
-- The top of the stack must be a homogeneous list, and the next token must be a quotation.
-- For each element in the list, the quotation is executed with the element as input,
-- and the result is collected into a new list.
--
-- == Examples:
--
-- >>> let block = TokVal (VQuotation [TokVal (VInt 10), TokOp OpMul])
-- >>> applyMapOp (initialStateWithStack [VList [VInt 1, VInt 2, VInt 3]] [block])
-- Right State{[[10,20,30]], []}
--
-- >>> applyMapOp (initialStateWithStack [VBool True] [TokVal (VQuotation [TokVal (VInt 1)])])
-- Left (ProgramError ExpectedList)
--
-- >>> applyMapOp (initialStateWithStack [VList [VInt 1]] [TokVal (VInt 1)])
-- Left (ProgramError ExpectedQuotation)
--
applyMapOp :: State -> Either BError State
applyMapOp st@State{ stack = list : _, program = block : _ } =
  case lookupValue st list of
    VList xs -> do
      st' <- popValue st
      let blockVal = checkBlock st block
      results <- traverse (applyBlock st' blockVal) xs
      pushValue (VList results) (nextToken st')
    _ -> Left $ ProgramError ExpectedList
applyMapOp State{ stack = [] } = Left $ ProgramError StackEmpty
applyMapOp st = Left . ProgramError $ ExpectedQuotation st

-- | Evaluates a `foldl` operation by applying a quotation to each element in a list
-- from left to right, carrying an accumulator.
--
-- The stack must contain a list and an initial accumulator. The the next 
-- program token must be a quotation that defines a binary operation.
-- The block is applied as:
--   acc0 list[0] -> acc1
--   acc1 list[1] -> acc2
--   ...
--   accN list[N] -> result
-- The final result is pushed back onto the stack.
--
-- == Examples:
--
-- >>> applyFoldlOp (initialStateWithStack [VInt 20, VList [VInt 2, VInt 5]] [TokVal (VQuotation [TokOp OpIDiv])])
-- Right State{[2], []}
--
-- >>> applyFoldlOp (initialStateWithStack [VList [VInt 2, VInt 5], VInt 20] [TokVal (VQuotation [TokOp OpIDiv])])
-- Left (ProgramError ExpectedVariable)
--
applyFoldlOp :: State -> Either BError State
applyFoldlOp st@State{ stack =  acc : list : _, program = block : _ } =
  case lookupValue st <$> [acc, list] of
    [VSymbol _, _, _] -> Left $ ProgramError UnknownSymbol
    [a, VList v] -> popValue (nextToken st) 
      >>= popValue 
      >>= pushValue a 
      >>= \initState ->
        foldM (foldStep (checkBlock st block)) initState v
    [_, VList _, _] -> Left . ProgramError $ ExpectedQuotation st
    _               -> Left . ProgramError $ ExpectedVariable st
  where
    foldStep q st' v =
      extractTop st' >>= \accVal ->
        popValue st' >>=                -- remove old accumulator
        pushValue accVal >>=            -- push it again
        pushValue v >>=                 -- push the current list element
        pushValue q >>=                 -- push the block
        execValue                       -- execute it, result becomes new accumulator
applyFoldlOp State{ stack = [] } = Left $ ProgramError StackEmpty
applyFoldlOp st = Left . ProgramError $ ExpectedQuotation st

-- | Ensures the given block is a quotation and applies it to a single value.
-- The value is pushed onto the stack, the quotation is set as the new program,
-- and the interpreter is run. The result is the top value from the resulting state.
--
-- Returns an error if the first argument is not a quotation.
--
applyBlock :: State -> Value -> Value -> Either BError Value
applyBlock st (VQuotation q) v =
  pushValue v st >>= (`withProgram` q) >>= interpret "" >>= extractTop
applyBlock st _ _ = Left . ProgramError $ ExpectedQuotation st

-- | Wraps a token into a quotation block, resolving variables if needed.
--
-- If the token is a value:
--   * If it's a quotation, it returns it directly.
--   * Otherwise, it resolves the value and wraps it in a singleton quotation.
--
-- If the token is an operation, it wraps it as a single-element quotation.
--
checkBlock :: State -> Token -> Value
checkBlock st tok = case tok of
  TokVal v -> case lookupValue st v of
    VQuotation q -> VQuotation q
    val            -> VQuotation [TokVal val]
  TokOp op -> VQuotation [TokOp op]

-- | Assigns a value to a symbol in the state dictionary. The first operation in the program must be `OpAssign`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a value.
-- The symbol is associated with the value in the dictionary.
--
assign :: (Value -> Maybe Value) -> (State -> ProgramError) -> State -> Either BError State
assign extract expectedError st@State{ stack =  val : VSymbol sym : rest } =
  maybe (Left . ProgramError $ expectedError st) 
        (updateDictionary sym) 
        (extract (lookupValue st val))
  where
    updateDictionary key var = Right st { stack = rest, dictionary = M.insert key var (dictionary st) }
assign _ _ st = Left . ProgramError $ ExpectedSymbol st

-- | Assigns a value to a symbol in the state dictionary. The first operation in the program must be `OpAssign`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a value.
-- The symbol is associated with the value in the dictionary.
--
-- == Examples:
--
-- >>> assignValue (initialStateWithStack [VInt 42, VSymbol "x"] [])
-- Right State{[], []}
--
-- >>> assignValue (initialStateWithStack [VQuotation [TokVal (VInt 1)], VSymbol "y"] [])
-- Right State{[], []}
--
-- >>> assignValue (initialStateWithStack [VInt 42, VInt 100] [])
-- Left (ProgramError ExpectedSymbol)
--
-- >>> let state = initialStateWithStack [VSymbol "z", VSymbol "b"] []
-- >>> assignValue state { dictionary = M.insert "z" (VInt 2) (dictionary state) }
-- Right State{[], []}
--
-- >>> assignValue (initialStateWithStack [VSymbol "z", VSymbol "b"] [])
-- Left (ProgramError ExpectedVariable)
--
assignValue :: State -> Either BError State
assignValue = assign noSymbols ExpectedVariable
  where
    noSymbols (VSymbol _) = Nothing
    noSymbols val         = Just val
 
-- | Assigns a function (quotation) to a symbol in the state dictionary. The first operation in the program must be `OpFun`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a 'VQuotation'.
-- The symbol is associated with the quotation in the dictionary.
--
-- == Examples:
--
-- >>> assignFunc (initialStateWithStack [VQuotation [TokVal (VInt 1)], VSymbol "func"] [])
-- Right State{[], []}
--
-- >>> assignFunc (initialStateWithStack [VInt 42, VSymbol "action"] [])
-- Left (ProgramError ExpectedQuotation)
--
-- >>> assignFunc (initialStateWithStack [VQuotation [TokVal (VInt 100)], VBool True] [])
-- Left (ProgramError ExpectedSymbol)
--
-- >>> assignFunc (initialStateWithStack [VInt 2, VSymbol "foo"] [])
-- Left (ProgramError ExpectedQuotation)
--
assignFunc :: State -> Either BError State
assignFunc = assign extractQuotation ExpectedQuotation
  where
    extractQuotation (VQuotation q) = Just (VQuotation q)
    extractQuotation _              = Nothing

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
-- Left (ProgramError StackEmpty)
--
execValue :: State -> Either BError State
execValue st@State{ stack = quote : _ } = 
  case lookupValue st quote of
    VQuotation q ->  popValue st >>= \st' -> 
      foldM (flip step) st' q
    _            -> Left . ProgramError $ ExpectedQuotation st
execValue State{ stack = [] } = Left $ ProgramError StackEmpty

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
pushValue (VList v) st = case updateList st (VList v) of
  Just xs  -> Right st { stack = xs : stack st }
  Nothing -> Left $ ProgramError ExpectedList
pushValue val st = Right st { stack = lookupValue st val : stack st }

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
  State{ stack = [_] }      -> Left . ProgramError $ ExpectedVariable st
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

-- | Extracts the top value from the stack of a given state.
-- 
-- This function checks if the stack has any elements. If so, it returns
-- the top element of the stack wrapped in a `Right`. If the stack is empty,
-- it returns a `Left` with a `ProgramError StackEmpty`.
-- 
-- == Examples:
--
-- >>> extractTop (initialStateWithStack [VInt 42] [])
-- Right 42
--
-- >>> extractTop (initialStateWithStack [] [])
-- Left (ProgramError StackEmpty)
extractTop :: State -> Either BError Value
extractTop st = case stack st of
  (top:_) -> Right $ lookupValue st top 
  _       -> Left $ ProgramError StackEmpty

-- | Updates each element of a list in the state.
-- 
-- This function takes a state and a list value (`VList`), and attempts to look up each element in the state.
-- If the value is of type `VList`, it applies the lookup function to each element of the list.
-- If the value is not a list, it returns `Nothing`.
-- 
updateList :: State -> Value -> Maybe Value
updateList st (VList xs) = Just . VList $ lookupValue st <$> xs 
updateList _ _ = Nothing

-- | Replaces the state's program with the given quotation.
--
-- This function takes the current state and a list of tokens (`[Token]`), and returns a new state where
-- the program is updated with the provided list of tokens. The function returns `Right` with the updated
-- state if the operation is successful, and an error is returned if any issues arise.
-- 
withProgram :: State -> [Token] -> Either BError State
withProgram st q = Right st { program = q }
