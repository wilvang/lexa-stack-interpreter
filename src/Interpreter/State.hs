{-# LANGUAGE InstanceSigs #-}
module Interpreter.State (State(..), Interrupt(..), lookupValue, setInterrupt, initialStateWithDict, initialStateWithStack) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Interpreter.Types (Value(..), Token(..))

-- | Dictionary maps symbols to values
type Dictionary = M.Map String Value

-- Define the IO interrupt type
data Interrupt = InputIO | OutputIO Value deriving Show 

-- | The interpreter state represents the full runtime context of the interpreter.
-- It tracks the program's progress, stores intermediate computation results,
-- and maintains definitions for variables and functions. It is designed to be
-- passed and updated throughout the evaluation process.
--
-- Fields:
--
-- [@stack@]      The evaluation stack, where intermediate values are pushed and popped 
--                during execution. Most operations manipulate this stack.
-- 
-- [@program@]    The remaining instructions to be executed. This field acts as the 
--                instruction pointer for the interpreter.
--
-- [@dictionary@] A mapping from variable names (as 'String') to their associated 'Value's. 
--                Used for storing user-defined variables, functions, and closures
--
-- [@printBuffer@] A list of output messages accumulated during execution. These are 
--                 collected for later use (e.g., displayed after pure evaluation).
--
data State = State
    {  
        stack :: [Value],
        program :: [Token],
        dictionary :: Dictionary,
        buffer :: [Interrupt]
    }

-- Custom Show instance for 'state', for printing the stack.
instance Show State where
    show :: State -> String
    show (State stk tokens _ _) = "State{" ++ show stk ++ ", " ++ show tokens ++ "}"

-- | Creates an initial interpreter state with a given dictionary.
-- This function initializes the interpreter state by setting the provided
-- dictionary of predefined variables and functions. The data stack, program,
-- and print buffer are initialized as empty.
--
-- Useful for starting execution in a preloaded environment, such as REPLs or
-- test scenarios where certain values or functions are already defined.
--
-- The resulting state has:
-- 1. An empty data stack.
-- 2. An empty instruction list (program).
-- 3. The provided dictionary.
-- 4. An empty print buffer.
--
initialStateWithDict :: M.Map String Value -> [Token] -> State
initialStateWithDict dict tokens = State
    { stack = []
    , program = tokens
    , dictionary = dict
    , buffer = []
    }

-- | Creates an interpreter state for testing with a predefined data stack.
--
-- This is useful for unit tests or debugging where you want to set the stack
-- manually without loading any program or dictionary entries.
--
-- The resulting state has:
-- 1. The given stack.
-- 2. An empty program.
-- 3. An empty dictionary.
-- 4. An empty print buffer.
--
initialStateWithStack :: [Value] -> [Token] -> State
initialStateWithStack stk tokens = State
    { stack = stk
    , program = tokens
    , dictionary = M.empty
    , buffer = []
    }

-- | Looks up values in the state's dictionary if they are symbols.
--
-- Each value in the input list is looked up in the state's dictionary:
-- if it is a 'VSymbol' and bound in the dictionary, its value is returned;
-- otherwise, the original value is returned unchanged.
--
-- This function is typically used to resolve a list of possibly symbolic values
-- before evaluation or pattern matching.
--
-- == Examples:
--
-- >>> let st = initialStateWithDict (M.fromList [("x", VInt 42)]) []
-- >>> lookupValue st (VSymbol "x")
-- 42
--
lookupValue :: State -> Value -> Value
lookupValue st val = fromMaybe val (lookupDict st val)

-- | Attempts to look up a symbol in the state's dictionary.
--
-- If the provided value is a 'VSymbol', it tries to retrieve its associated
-- value from the state's dictionary. Returns 'Nothing' if the symbol is
-- unbound or the value is not a symbol.
--
-- == Examples:
--
-- >>> let st = initialStateWithDict (M.fromList [("y", VInt 10)]) []
-- >>> lookupDict st (VSymbol "y")
-- Just 10
--
-- >>> lookupDict st (VInt 1)
-- Nothing
--
lookupDict :: State -> Value -> Maybe Value
lookupDict st (VSymbol var) = Just =<< M.lookup var (dictionary st)
lookupDict _ _ = Nothing

-- | Adds an 'Interrupt' to the front of the state's buffer.
--
-- This is used to signal that the interpreter should pause and handle
-- an external effect like input/output.
--
setInterrupt :: Interrupt -> State -> State
setInterrupt trap st@State{ buffer = rest } = st { buffer = trap : rest }