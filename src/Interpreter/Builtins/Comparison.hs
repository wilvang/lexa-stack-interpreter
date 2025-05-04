module Interpreter.Builtins.Comparison (safeEQ, safeLT, safeGT) where

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


-- | Safely checks if the first numeric 'Value' is less than the second.
--
-- Performs numeric comparison between 'VInt' and 'VFloat' values,
-- converting integers to floating-point as needed.
--
-- == Examples
--
-- >>> safeLT (VInt 2) (VFloat 3.0)
-- Right True
--
-- >>> safeLT (VFloat 4.5) (VInt 2)
-- Right False
--
-- >>> safeLT (VString "a") (VInt 1)
-- Left (ProgramError NotComparable)
safeLT :: Value -> Value -> Either BError Value
safeLT = compareNumeric (<)


-- | Safely checks if the first numeric 'Value' is greater than the second.
--
-- Performs numeric comparison between 'VInt' and 'VFloat' values,
-- converting integers to floating-point as needed.
--
-- == Examples
--
-- >>> safeGT (VFloat 5.5) (VInt 5)
-- Right True
--
-- >>> safeGT (VInt 1) (VInt 10)
-- Right False
--
-- >>> safeGT (VBool True) (VInt 1)
-- Left (ProgramError NotComparable)
safeGT :: Value -> Value -> Either BError Value
safeGT = compareNumeric (>)

-- | Compares two numeric 'Value's using the given numeric comparison function.
--
-- Automatically converts between 'VInt' and 'VFloat' for safe comparison.
-- Returns an error if the values are not numeric.
--
-- == Examples
--
-- >>> compareNumeric (<) (VInt 2) (VFloat 3.5)
-- Right True
--
-- >>> compareNumeric (>) (VFloat 5.0) (VInt 10)
-- Right False
--
-- >>> compareNumeric (<) (VString "a") (VInt 1)
-- Left (ProgramError NotComparable)
--
compareNumeric :: (Double -> Double -> Bool) -> Value -> Value -> Either BError Value
compareNumeric f a b = case (a, b) of
  (VInt x, VInt y)     -> Right . VBool $ f (fromIntegral x) (fromIntegral y)
  (VInt x, VFloat y)   -> Right . VBool $ f (fromIntegral x) y
  (VFloat x, VInt y)   -> Right . VBool $ f x (fromIntegral y)
  (VFloat x, VFloat y) -> Right . VBool $ f x y
  _                    -> Left (ProgramError NotComparable)
