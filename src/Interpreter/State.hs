module Interpreter.State (State(..), initialStateWithDict, initialStateWithStack) where

import qualified Data.Map as M
import Interpreter.Types (Value(..), Token(..))

-- | Dictionary maps symbols to values
type Dictionary = M.Map String Value


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
        printBuffer :: [String]
    }

-- Custom Show instance for 'state', for printing the stack.
instance Show State where
    show (State stk _ _ _) = show stk


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
    , printBuffer = []
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
initialStateWithStack :: [Value] -> State
initialStateWithStack stk = State
    { stack = stk
    , program = []
    , dictionary = M.empty
    , printBuffer = []
    }
