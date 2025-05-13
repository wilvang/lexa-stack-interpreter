module Interpreter.Eval.Utils.ProgramUtils (assignValue, assignFunc, extractTop, nextToken, withProgram) where

import qualified Data.Map as M
import Interpreter.State (State(..), lookupValue)
import Interpreter.Types (Token(..), Value(..))
import Interpreter.Error (BError(..), ProgramError(..))

-- | Assigns a value to a symbol in the state dictionary. The first operation in the program must be `OpAssign`.
--
-- The top of the stack must be a 'VSymbol', and the next token in the program must be a value.
-- The symbol is associated with the value in the dictionary.
--
-- $setup
-- >>> import Interpreter.State ( initialStateWithStack )
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

-- | Replaces the state's program with the given quotation.
--
-- This function takes the current state and a list of tokens (`[Token]`), and returns a new state where
-- the program is updated with the provided list of tokens. The function returns `Right` with the updated
-- state if the operation is successful, and an error is returned if any issues arise.
-- 
withProgram :: State -> [Token] -> Either BError State
withProgram st q = Right st { program = q }