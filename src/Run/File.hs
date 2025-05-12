module Run.File (runFile, importLib) where

import Control.Monad (void)
import qualified Data.Map as M
import Run.Program (evalProgram)
import Interpreter.Eval (interpret)
import Interpreter.State (State(..), initialStateWithStack, initialStateWithDict)

-- | Executes a program from a file.
--
-- This function reads the contents of the file at the given @path@ and evaluates
-- it as a program using the provided initial @State@. It interprets the program,
-- handles any I/O operations (such as input or output), and executes side effects
-- accordingly. The resulting state is discarded, meaning the purpose of this function
-- is for side effects, not further computation.
--
-- == Parameters:
--
-- * @path@ - The file path to the source code to be executed.
-- * @st@   - The initial state to use for evaluating the program.
--
-- == Effects:
--
-- * May print to stdout or read from stdin depending on program behavior.
-- * Does not return the final state.
--
runFile :: String -> State -> IO ()
runFile path st = readFile path >>= \prog ->
    void $ evalProgram prog st

-- | Imports a dictionary of bindings from a library string.
--
-- This function interprets the given program source code (@lib@), assuming it
-- contains only dictionary definitions (words/macros). It extracts the resulting
-- dictionary and returns a new initial state populated with these definitions.
--
-- If interpretation fails, it falls back to returning an empty dictionary.
--
-- == Parameters:
--
-- * @lib@ - The source code string containing dictionary definitions.
--
-- == Returns:
--
-- * A new initial state containing the interpreted dictionary.
--
importLib :: String -> State
importLib lib =  
    case interpret lib (initialStateWithStack [] []) of
        Right State{ dictionary = dict } -> initialStateWithDict dict []
        Left _                           -> initialStateWithDict M.empty []