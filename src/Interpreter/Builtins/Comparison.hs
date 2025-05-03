module Interpreter.Builtins.Comparison (safeEQ) where

import Interpreter.Types
import Interpreter.Error (BError(..), ProgramError(..))

-- | Safely compares two 'Value's for equality.
--
-- Accepts comparisons between:
--   * Int and Int
--   * Int and Float (and vice versa)
--   * Float and Float
--   * String and String
--   * Bool and Bool
--
-- Returns a 'VBool' wrapped in 'Right' if the values are comparable,
-- otherwise returns 'Left' with a 'ProgramError'.
--
-- == Examples
--
-- >>> safeEQ (VInt 3) (VInt 3)
-- Right True
--
-- >>> safeEQ (VInt 3) (VFloat 3.0)
-- Right True
--
-- >>> safeEQ (VString "hello") (VString "world")
-- Right False
--
-- >>> safeEQ (VInt 1) (VBool True)
-- Left (ProgramError NotComparable)
safeEQ :: Value -> Value -> Either BError Value
safeEQ a b = case (a, b) of
  (VInt _, VInt _)       -> Right . VBool $ a == b
  (VInt x, VFloat _)     -> Right . VBool $ VFloat(fromIntegral x) == b
  (VFloat _, VInt y)     -> Right . VBool $ a == VFloat(fromIntegral y)
  (VFloat _, VFloat _)   -> Right . VBool $ a == b
  (VString _, VString _) -> Right . VBool $ a == b
  (VBool _, VBool _)     -> Right . VBool $ a == b
  _                      -> Left (ProgramError NotComparable)