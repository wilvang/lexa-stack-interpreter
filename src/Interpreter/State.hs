module Interpreter.State (State, initialStateWithDict) where

import qualified Data.Map as M
import Interpreter.Types (Value(..), Token(..))

-- | Dictionary maps symbols to values
type Dictionary = M.Map String Value

-- | Stack stores values and results
type Stack = [Value]

-- | Instructions contains tokens to be executed
type Instruction = [Token]


-- | The interpreter state represents the full runtime context of the interpreter.
-- It tracks the program's progress, stores intermediate computation results,
-- and maintains definitions for variables and functions. It is designed to be
-- passed and updated throughout the evaluation process.
--
-- Fields:
--
-- [@dictionary@] A mapping from variable names (as 'String') to their associated 'Value's. 
--                Used for storing user-defined variables, functions, and closures.
--
-- [@stack@]      The evaluation stack, where intermediate values are pushed and popped 
--                during execution. Most operations manipulate this stack.
--
-- [@program@]    The remaining instructions to be executed. This field acts as the 
--                instruction pointer for the interpreter.
--
-- [@printBuffer@] A list of output messages accumulated during execution. These are 
--                 collected for later use (e.g., displayed after pure evaluation).
--
data State = State
    { 
        dictionary :: Dictionary, 
        stack :: Stack,
        program :: Instruction,
        printBuffer :: [String]
    }

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
initialStateWithDict :: M.Map String Value -> State
initialStateWithDict dict = State
    { dictionary = dict
    , stack = []
    , program = []
    , printBuffer = []
    }