module Run.REPL (repl, startMessage) where

import System.IO (hFlush, stdout)
import Run.Program (evalProgram, eol)
import Interpreter.State (State(..))

-- | Starts the REPL (Read-Eval-Print Loop) for Lexa.
-- This function initiates an interactive REPL session for the Lexa interpreter.
-- It takes the current state (`State`) as input, then processes user commands
-- and interacts with the user by printing results or responding to commands.
--
-- == Behavior:
-- The REPL reads the user input in a loop, evaluates it, and displays the result.
-- It responds to special meta-commands (commands starting with `:`) for various 
-- actions like displaying help, exiting, or showing internal state information.
-- The REPL continues prompting the user until the user exits.
--
-- == Meta-commands:
--
-- * `:b` - Prints the current variable bindings (dictionary).
-- * `:s` - Prints the current operand stack.
-- * `:q` - Exits the REPL.
-- * `:?` - Displays the help menu with available commands.
--
repl :: State -> IO ()
repl st = putStr prompt >> hFlush stdout >> getLine >>= \input -> 
  case input of
    ":?" -> putStrLn help >> repl st
    ":q" -> putStrLn exitMessage
    ":s" -> print (stack st) >> eol >>repl st
    ":b" -> print (dictionary st) >> eol >> repl st
    ""   -> repl st  -- skip empty input
    _    -> evalProgram input st >>= \newState ->
        repl newState