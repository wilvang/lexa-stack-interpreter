module Interpreter.Builtins.Logic (safeOr, safeAnd, safeNot) where 

import Interpreter.Types (Value(..))
import Interpreter.Error (BError(..), ProgramError(..))


-- | Safely perform a logical OR on two boolean values.
--
-- >>> safeOr (VBool True) (VBool False)
-- Right True
--
-- >>> safeOr (VInt 1) (VBool False)
-- Left (ProgramError ExpectedBool)
--
safeOr :: Value -> Value -> Either BError Value
safeOr (VBool a) (VBool b) = Right . VBool $ a || b
safeOr _ _ = Left $ ProgramError ExpectedBool

-- | Safely perform a logical AND on two boolean values.
--
-- >>> safeAnd (VBool True) (VBool False)
-- Right False
--
-- >>> safeAnd (VInt 1) (VBool False)
-- Left (ProgramError ExpectedBool)
--
safeAnd :: Value -> Value -> Either BError Value
safeAnd (VBool a) (VBool b) = Right . VBool $ a && b
safeAnd _ _ = Left $ ProgramError ExpectedBool

-- | Perform logical negation on a boolean or numeric negation on an int/float.
--
-- >>> safeNot (VBool True)
-- Right False
--
-- >>> safeNot (VInt 3)
-- Right -3
--
-- >>> safeNot (VFloat 1.5)
-- Right -1.5
--
-- >>> safeNot (VString "oops")
-- Left (ProgramError ExpectedBoolOrNumber)
--
safeNot :: Value -> Either BError Value
safeNot a = case a of
    (VBool x)  -> Right . VBool $ not x
    (VFloat x) -> Right . VFloat $ -x
    (VInt x)   -> Right . VInt $ -x
    _          -> Left $ ProgramError ExpectedBoolOrNumber
