module Run.File (runFile) where

import Control.Monad (void)
import Run.Program (evalProgram)
import Interpreter.State (State(..))

-- | Executes a program stored in a file.
--
-- This function reads the contents of the file at the given path and evaluates
-- it as a program using the provided initial state. It interprets the program,
-- handles any I/O interrupts, and prints results or errors as appropriate.
--
-- The resulting state is discarded, but any side effects such as printed output
-- or user input are executed during interpretation.
--
-- == Parameters:
--
-- * @path@ - The path to the file containing the program source code.
-- * @st@   - The initial state used for evaluation.
--
runFile :: String -> State -> IO ()
runFile path st = readFile path >>= \prog ->
    void $ evalProgram prog st