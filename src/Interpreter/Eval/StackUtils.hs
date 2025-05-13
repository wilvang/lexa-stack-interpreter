module Interpreter.Eval.StackUtils (pushValue, popValue, dupValue, swapValue) where

import Interpreter.State (State(..), lookupValue)
import Interpreter.Types (Value(..))
import Interpreter.Error (BError(..), ProgramError(..))

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

-- | Updates each element of a list in the state.
-- 
-- This function takes a state and a list value (`VList`), and attempts to look up each element in the state.
-- If the value is of type `VList`, it applies the lookup function to each element of the list.
-- If the value is not a list, it returns `Nothing`.
-- 
updateList :: State -> Value -> Maybe Value
updateList st (VList xs) = Just . VList $ lookupValue st <$> xs 
updateList _ _ = Nothing