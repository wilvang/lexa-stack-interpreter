-- Stack machine evaluator
module Interpreter.Eval (interpret) where

import Interpreter.Types (Token(..), Op(..), Value(..))
import Interpreter.Error (ProgramError(..), BError(..))
import Interpreter.Parser

type Stack = [Value]


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
interpret :: String -> Either BError Stack
interpret input = case parseTokens input of
      Left err     -> Left err
      Right tokens -> process tokens []
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: [Token] -> Stack -> Either BError Stack
    process [] stack = Right stack  -- Base case: no more tokens, return the final stack
    process (token : tokens) stack = 
      case step token stack of
        Left err       -> Left err  -- If step produces an error, propagate it
        Right newStack -> process tokens newStack  -- Continue processing with the new stack


step :: Token -> Stack -> Either BError Stack
step token stack = -- todo...