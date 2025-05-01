-- Stack machine evaluator
module Interpreter.Eval where

import Text.Read (readMaybe)
import Interpreter.Types
import Interpreter.Error (ParserError)
import Interpreter.Parser 

type Stack = [Value]


-- | Interprets a program given as a string.
-- Returns either a ParserError if tokenization fails,
-- or the final Stack after evaluating all tokens.
interpret :: String -> Either ParserError Stack
interpret input = case parseTokens input of
      Left err     -> Left err
      Right tokens -> Right (process tokens [])
  where
    -- | Recursively processes a list of tokens, updating the stack at each step.
    process :: [Token] -> Stack -> Stack
    process [] stack = stack  -- Base case: no more tokens, return the final stack
    process (token : tokens) stack = process tokens (step token stack)  -- Apply one token, then continue


step :: Token -> Stack -> Stack
step token stack = -- todo...

