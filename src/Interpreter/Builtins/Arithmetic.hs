module Interpreter.Builtins.Arithmetic where

import Interpreter.Error( ProgramError(ExpectedBoolOrNumber, DivisionByZero), BError (ProgramError) )
import Interpreter.Types    

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
