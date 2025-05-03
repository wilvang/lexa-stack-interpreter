module Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul, safeDiv, safeIntDiv) where

import Interpreter.Types
import Interpreter.Error( ProgramError(ExpectedBoolOrNumber, DivisionByZero), BError (ProgramError) )


-- | Safely adds two values.
-- Handles both integer and floating-point addition. 
-- Returns an error if the types are not compatible.
-- 
-- == Examples:
-- 
-- >>> safeAdd (VInt 2) (VInt 3)
-- Right 5
-- 
-- >>> safeAdd (VFloat 2.0) (VFloat 3.0)
-- Right 5.0
-- 
-- >>> safeAdd (VInt 2) (VFloat 3.0)
-- Right 5.0
-- 
-- >>> safeAdd (VBool True) (VInt 3)
-- Right 4
safeAdd :: Value -> Value -> Either BError Value
safeAdd = liftNumeric (+)(+)

-- | Safely subtracts two values.
-- Handles both integer and floating-point subtraction.
-- Returns an error if the types are not compatible.
-- 
-- == Examples:
-- 
-- >>> safeSub (VInt 5) (VInt 3)
-- Right 2
-- 
-- >>> safeSub (VFloat 5.5) (VFloat 3.0)
-- Right 2.5
-- 
-- >>> safeSub (VInt 10) (VFloat 3.5)
-- Right 6.5
-- 
-- >>> safeSub (VString "hello") (VInt 1)
-- Left (ProgramError ExpectedBoolOrNumber)
safeSub :: Value -> Value -> Either BError Value
safeSub = liftNumeric (-)(-)

-- | Safely multiplies two values.
-- Handles both integer and floating-point multiplication.
-- Returns an error if the types are not compatible.
-- 
-- == Examples:
-- 
-- >>> safeMul (VInt 4) (VInt 5)
-- Right 20
-- 
-- >>> safeMul (VFloat 2.0) (VFloat 3.0)
-- Right 6.0
-- 
-- >>> safeMul (VInt 2) (VFloat 3.0)
-- Right 6.0
-- 
-- >>> safeMul (VBool True) (VInt 3)
-- Right 3
safeMul :: Value -> Value -> Either BError Value
safeMul = liftNumeric (*)(*)

-- | Safely performs division on two values.
-- Handles division by zero and type mismatch errors.
-- 
-- == Examples:
-- 
-- >>> safeDiv (VInt 10) (VInt 2)
-- Right 5.0
-- 
-- >>> safeDiv (VFloat 10.0) (VFloat 2.0)
-- Right 5.0
-- 
-- >>> safeDiv (VInt 10) (VInt 0)
-- Left (ProgramError DivisionByZero)
-- 
-- >>> safeDiv (VBool True) (VInt 2)
-- Right 0.5
-- 
-- >>> safeDiv (VInt 10) (VString "hello")
-- Left (ProgramError ExpectedBoolOrNumber)
safeDiv :: Value -> Value -> Either BError Value
safeDiv a b = case (toFloat a, toFloat b) of
    (Nothing, _) -> Left . ProgramError $ ExpectedBoolOrNumber  -- Non-numeric 'a'
    (_, Nothing) -> Left . ProgramError $ ExpectedBoolOrNumber  -- Non-numeric 'b'
    (_, Just 0.0) -> Left . ProgramError $ DivisionByZero  -- Division by zero
    (Just x, Just y) -> Right . VFloat $ x / y  -- Safe float division

-- | Safely performs integer division on two values.
-- Handles division by zero and type mismatch errors.
-- 
-- == Examples:
-- 
-- >>> safeIntDiv (VInt 10) (VInt 2)
-- Right 5
-- 
-- >>> safeIntDiv (VFloat 10.0) (VFloat 2.0)
-- Right 5
-- 
-- >>> safeIntDiv (VInt 10) (VInt 0)
-- Left (ProgramError DivisionByZero)
-- 
-- >>> safeIntDiv (VBool True) (VInt 2)
-- Right 0
-- 
-- >>> safeIntDiv (VInt 10) (VString "hello")
-- Left (ProgramError ExpectedBoolOrNumber)
safeIntDiv :: Value -> Value -> Either BError Value
safeIntDiv a b = case (toInt a, toInt b) of
    (Nothing, _) -> Left . ProgramError $ ExpectedBoolOrNumber  -- Non-numeric 'a'
    (_, Nothing) -> Left . ProgramError $ ExpectedBoolOrNumber  -- Non-numeric 'b'
    (_, Just 0) -> Left . ProgramError $ DivisionByZero  -- Division by zero
    (Just x, Just y) -> Right . VInt $ x `div` y -- Safe integer division

-- | Converts a 'Value' to an 'Int'.
-- 
-- Handles:
-- - 'VInt': returns the 'Int' directly.
-- - 'VFloat': truncates the 'Float' to an 'Int' (drops the decimal part).
-- - 'VBool': converts 'True' to 1 and 'False' to 0.
-- 
-- Returns 'Nothing' for any other type of 'Value'.
-- 
-- == Examples:
-- 
-- >>> toInt (VInt 5)
-- Just 5
-- 
-- >>> toInt (VFloat 3.14)
-- Just 3
-- 
-- >>> toInt (VBool True)
-- Just 1
-- 
-- >>> toInt (VBool False)
-- Just 0
-- 
-- >>> toInt (VString "hello")
-- Nothing
toInt :: Value -> Maybe Int
toInt (VInt a) = Just a
toInt (VFloat a) = Just $ truncate a
toInt (VBool a) = Just $ if a then 1 else 0
toInt _ = Nothing


-- | Converts a 'Value' to a 'Double' (floating-point number).
-- 
-- Handles:
-- - 'VInt': converts the 'Int' to a 'Double' using 'fromIntegral'.
-- - 'VFloat': returns the 'Float' as a 'Double'.
-- - 'VBool': converts 'True' to 1.0 and 'False' to 0.0.
-- 
-- Returns 'Nothing' for any other type of 'Value'.
-- 
-- == Examples:
-- 
-- >>> toFloat (VInt 5)
-- Just 5.0
-- 
-- >>> toFloat (VFloat 3.14)
-- Just 3.14
-- 
-- >>> toFloat (VBool True)
-- Just 1.0
-- 
-- >>> toFloat (VBool False)
-- Just 0.0
-- 
-- >>> toFloat (VString "hello")
-- Nothing
toFloat :: Value -> Maybe Double
toFloat (VInt a) = Just $ fromIntegral a
toFloat (VFloat a) = Just a
toFloat (VBool a) = Just $ if a then 1 else 0
toFloat _ = Nothing

-- | Converts a `Value` to a numeric type (`Int` or `Double`).
-- Returns `Nothing` if the value cannot be converted.
-- 
-- == Examples:
-- 
-- >>> toNumeric (VInt 5)
-- Just (Left 5)
-- 
-- >>> toNumeric (VFloat 3.14)
-- Just (Right 3.14)
-- 
-- >>> toNumeric (VBool True)
-- Just (Left 1)
-- 
-- >>> toNumeric (VBool False)
-- Just (Left 0)
-- 
-- >>> toNumeric (VString "hello")
-- Nothing
toNumeric :: Value -> Maybe (Either Int Double)
toNumeric (VFloat x) = Just . Right $ x
toNumeric (VInt x)   = Just . Left $ x
toNumeric (VBool x)  = Just . Left $ if x then 1 else 0
toNumeric _          = Nothing

-- | A generic function that lifts an integer and a floating-point operation.
-- The function handles both integer and floating-point values.
-- Returns an error if the types do not match.
-- 
-- == Examples:
-- 
-- >>> liftNumeric (+) (+) (VInt 3) (VInt 4)
-- Right 7
-- 
-- >>> liftNumeric (+) (+) (VBool True) (VInt 4)
-- Right 5
--
-- >>> liftNumeric (+) (+) (VFloat 3.0) (VFloat 4.0)
-- Right 7.0
-- 
-- >>> liftNumeric (+) (+) (VInt 3) (VFloat 4.3)
-- Right 7.3
-- 
-- >>> liftNumeric (+) (+) (VBool True) (VString "hei")
-- Left (ProgramError ExpectedBoolOrNumber)
liftNumeric :: (Int -> Int -> Int) -> (Double -> Double -> Double)
            -> Value -> Value -> Either BError Value
liftNumeric opInt opFloat a b =
  case (toNumeric a, toNumeric b) of
    (Just (Left x), Just (Left y)) -> Right . VInt $ opInt x y
    (Just (Right x), Just (Right y)) -> Right . VFloat $ opFloat x y
    (Just (Left x), Just (Right y)) -> Right . VFloat $ opFloat (fromIntegral x) y
    (Just (Right x), Just (Left y)) -> Right . VFloat $ opFloat x (fromIntegral y)
    _ -> Left . ProgramError $ ExpectedBoolOrNumber