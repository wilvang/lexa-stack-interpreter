{-# LANGUAGE LambdaCase #-}
module Interpreter.Builtins.List (safeHead, safeTail, safeEmpty, safeLength) where

import Interpreter.Types (Value(..))
import Interpreter.Error (BError(ProgramError), ProgramError(..))
import Control.Monad (guard)

-- | Safely retrieves the head of a homogeneous list.
-- Returns an error if the list is empty or not homogeneous.
--
-- == Examples:
--
-- >>> safeHead (VList [VInt 1, VInt 1, VInt 1])
-- Right 1
--
-- >>> safeHead (VList [])
-- Left (ProgramError EmptyList)
--
-- >>> safeHead (VList [VInt 1, VString "oops"])
-- Left (ProgramError ExpectedList)
--
safeHead :: Value -> Either BError Value
safeHead = (`withValidatedList` \case
    []    -> Left $ ProgramError EmptyList
    (x:_) -> Right x)

-- | Safely retrieves the tail of a homogeneous list.
-- Returns an error if the list is empty or not homogeneous.
--
-- == Examples:
--
-- >>> safeTail (VList [VInt 1, VInt 1, VInt 1])
-- Right [ 1 1 ]
--
-- >>> safeTail (VList [VInt 42])
-- Right [  ]
--
-- >>> safeTail (VList [])
-- Left (ProgramError EmptyList)
--
-- >>> safeTail (VList [VInt 1, VBool True])
-- Left (ProgramError ExpectedList)
--
safeTail :: Value -> Either BError Value
safeTail = (`withValidatedList` \case
    []     -> Left $ ProgramError EmptyList
    (_:xs) -> Right $ VList xs)

-- | Checks if a homogeneous list is empty.
-- Returns a boolean wrapped in 'VBool'.
--
-- == Examples:
--
-- >>> safeEmpty (VList [])
-- Right True
--
-- >>> safeEmpty (VList [VInt 5])
-- Right False
--
-- >>> safeEmpty (VList [VInt 1, VString "oops"])
-- Left (ProgramError ExpectedList)
--
safeEmpty :: Value -> Either BError Value
safeEmpty = (`withValidatedList` Right . VBool . null)

-- | Computes the length of a homogeneous list.
-- Returns the length wrapped in 'VInt'.
--
-- == Examples:
--
-- >>> safeLength (VList [])
-- Right 0
--
-- >>> safeLength (VList [VInt 1, VInt 1, VInt 1])
-- Right 3
--
-- >>> safeLength (VList [VInt 1, VBool True])
-- Left (ProgramError ExpectedList)
safeLength :: Value -> Either BError Value
safeLength = (`withValidatedList` Right . VInt . length)

-- | Applies a function to a validated homogeneous list.
-- Returns an error if the input is not a valid homogeneous list.
--
-- == Examples:
--
-- >>> withValidatedList (VList [VInt 1, VInt 1]) (Right . VInt . length)
-- Right 2
--
-- >>> withValidatedList (VList [VInt 1, VBool False]) (Right . VInt . length)
-- Left (ProgramError ExpectedList)
--
withValidatedList :: Value -> ([Value] -> Either BError Value) -> Either BError Value
withValidatedList val f = case validateElements val of
    Just (VList xs) -> f xs
    _               -> Left $ ProgramError ExpectedList

-- | Validates that all elements in a 'VList' are of the same value.
-- Returns 'Just' the original list if homogeneous, otherwise 'Nothing'.
--
-- == Examples:
--
-- >>> validateElements (VList [VInt 1, VInt 1])
-- Just [ 1 1 ]
--
-- >>> validateElements (VList [])
-- Just [  ]
--
-- >>> validateElements (VList [VInt 1, VString "oops"])
-- Nothing
--
validateElements :: Value -> Maybe Value
validateElements (VList xs@(x:rest)) = do
    guard (all (== x) rest)
    Just (VList xs)
validateElements v@(VList []) = Just v
validateElements _ = Nothing