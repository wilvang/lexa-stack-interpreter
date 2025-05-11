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

-- | Message displayed when the REPL is started.
--
-- This message informs the user that the REPL has started and provides the user 
-- with basic instructions on how to access the help menu.
--
startMessage :: String
startMessage = "This is Lexa REPL-mode,  :? for help\n"

-- | Message displayed when the user exits the REPL.
--
-- This message is shown when the user exits the REPL-mode by entering the `:q` 
-- command.
--
exitMessage :: String
exitMessage = "Exiting Lexa REPL."

-- | Prompt string displayed to the user in the REPL.
--
-- This string is displayed every time the REPL is ready to accept user input.
-- It serves as a visual cue to indicate that the interpreter is waiting for 
-- further instructions from the user.
--
prompt :: String
prompt = "lexa> "

-- | Help message displaying available meta-commands.
--
-- This message provides a list of all available commands that the user can 
-- use within the REPL. It helps the user understand how to interact with 
-- the REPL and perform actions such as viewing the operand stack, 
-- variable bindings, or exiting the REPL.
--
help :: String
help = "Meta commands:\n" 
    ++ ":b - prints internal variable bindings\n" 
    ++ ":s - prints internal operand stack\n"
    ++ ":q - exits the REPL-mode\n"
    ++ ":? - shows the help menu\n"
