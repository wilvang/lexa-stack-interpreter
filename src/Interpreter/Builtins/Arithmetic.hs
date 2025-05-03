module Interpreter.Builtins.Arithmetic (safeAdd, safeSub, safeMul) where

import Interpreter.Error( ProgramError(ExpectedBoolOrNumber, DivisionByZero), BError (ProgramError) )
import Interpreter.Types    

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
