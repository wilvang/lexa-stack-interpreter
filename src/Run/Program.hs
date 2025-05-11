module Run.Program (evalProgram, interpretIO, eol) where

import Interpreter.State (State(..), Interrupt(..))
import Interpreter.Types (Value(..))
import Interpreter.Eval (interpret)

-- | Evaluates a program and processes the state of the stack after execution.
--
-- This function interprets a given input string, handles any IO interrupts, 
-- and then checks the state of the stack after the program execution.
-- It prints messages based on the state of the stack and returns the updated state.
--
-- The function performs the following actions based on the state of the stack:
--
-- == Behavior:
--
-- * If there are multiple values on the stack, it prints an error message `-- multiple values on the stack --` 
--   and returns the current state. This is an error condition, as only one value is expected on the stack after execution.
-- * If there is exactly one value on the stack, it prints that value and returns the updated state.
-- * If the stack is empty, it prints `-- empty stack --` and returns the current state. 
--   This indicates that the program was unable to produce a result.
--
evalProgram :: String -> State -> IO State
evalProgram input st = interpretIO input st >>= \st' -> 
  case stack st' of
    -- Case when there are multiple values on the stack
    _:_:_ -> print (stack st') >> putStr errorMultipleValues >> return st'

    -- Case when there is exactly one value on the stack
    x:_ -> print x >> eol >> return st'

    -- Case when the stack is empty
    [] -> print (stack st') >> putStr errorEmptyStack >> return st'

-- | Handles IO interrupts during program execution.
-- This function processes the given program string and state, and
-- performs the necessary input/output actions for handling
-- `OutputIO` and `InputIO` interrupts, while continuing the program execution.
--
-- The function is used to interpret a program, handling IO operations
-- such as printing values to the console (`OutputIO`) and reading
-- user input (`InputIO`), before continuing with the execution of the program.
--
-- == Behavior:
--
-- If the `interpret` function produces an error, it prints the error
-- and returns the current state without any changes.
--
-- If the result of `interpret` includes an `OutputIO` interrupt, it will
-- print the corresponding value to the console and continue the program
-- execution after the output operation.
--
-- If the result of `interpret` includes an `InputIO` interrupt, it will
-- prompt the user for input (via `getLine`), add the input to the stack,
-- and continue execution with the updated state.
--
interpretIO :: String -> State -> IO State
interpretIO s st = case interpret s st of
  Left err -> print err >> eol >> return st
  Right newSt -> case buffer newSt of
    (OutputIO val : rest) -> print val >> eol >> interpretIO "" newSt { buffer = rest }
    (InputIO : rest) -> getLine >>= \input ->
        interpretIO "" newSt { stack = VString input : stack newSt, buffer = rest }
    _   -> return newSt

-- | Error message to indicate that there are multiple values on the stack.
--
-- This string is used to signal that the stack contains more than one value,
-- which is an unexpected state when the program is expected to have only one value
-- at the top of the stack after execution.
--
errorMultipleValues :: String
errorMultipleValues = "-- multiple values on the stack --\n\n"

-- | Error message to indicate that the stack is empty.
--
-- This string is used to signal that the stack is empty when the program expects
-- at least one value to be present on the stack. This situation may occur due to
-- incorrect execution flow or an unexpected state.
--
errorEmptyStack :: String
errorEmptyStack = "-- empty stack --\n\n"

-- | Writes a newline character to the standard output.
--
-- This is a convenience function used to print an end-of-line,
-- equivalent to 'putStr "\\n"'. It can be used to improve code
-- readability when printing newlines explicitly.
--
eol :: IO ()
eol = putStr "\n"
