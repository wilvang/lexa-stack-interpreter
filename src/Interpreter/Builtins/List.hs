{-# LANGUAGE LambdaCase #-}
module Interpreter.Builtins.List (safeHead, safeTail, safeEmpty, safeLength, safeCons, safeAppend) where

import Interpreter.Types (Value(..), sameConstructor)
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

-- | Safely prepends an element to a homogeneous list.
-- If the list is empty, any element is allowed.
-- If the list is non-empty, the new element must match the existing elements to preserve homogeneity.
-- Returns an error if the types/values are incompatible.
--
-- == Examples:
--
-- >>> safeCons (VInt 0) (VList [VInt 1, VInt 1])
-- Right [ 0 1 1 ]
--
-- >>> safeCons (VBool True) (VList [])
-- Right [ True ]
--
-- >>> safeCons (VString "a") (VList [VString "b", VString "c"])
-- Right [ "a" "b" "c" ]
--
-- >>> safeCons (VInt 1) (VList [VBool True])
-- Left (ProgramError ExpectedList)
--
-- >>> safeCons (VInt 1) (VInt 2)
-- Left (ProgramError ExpectedList)
--
safeCons :: Value -> Value -> Either BError Value
safeCons v lst = withValidatedList lst $ \xs ->
  case xs of
    []    -> Right $ VList [v]  -- empty list: accept any element
    (x:_) -> if sameConstructor x v          -- check homogeneity
             then Right $ VList (v : xs)
             else Left $ ProgramError ExpectedList

-- | Safely appends two homogeneous lists.
-- If either list is empty, it returns the non-empty list.
-- If both are non-empty, it checks that the elements are of the same type/value.
-- Returns an error if they are not compatible.
--
-- == Examples:
--
-- >>> safeAppend (VList [VInt 1, VInt 1]) (VList [VInt 1, VInt 1])
-- Right [ 1 1 1 1 ]
--
-- >>> safeAppend (VList []) (VList [VBool True])
-- Right [ True ]
--
-- >>> safeAppend (VList [VString "a"]) (VList [])
-- Right [ "a" ]
--
-- >>> safeAppend (VList [VInt 1]) (VList [VString "oops"])
-- Left (ProgramError ExpectedList)
--
safeAppend :: Value -> Value -> Either BError Value
safeAppend l1 l2 = extract l1 >>= \xs -> extract l2 >>= \ys ->
        appendIfCompatible xs ys

-- | Helper function to concatenate two validated homogeneous lists.
-- If both lists are non-empty, checks that the first elements match.
-- If the check passes, concatenates them. Otherwise, returns an error.
--
-- == Examples:
--
-- >>> appendIfCompatible [VInt 1] [VInt 2]
-- Right [ 1 2 ]
--
-- >>> appendIfCompatible [VInt 1] [VBool True]
-- Left (ProgramError ExpectedList)
--
-- >>> appendIfCompatible [] [VBool False]
-- Right [ False ]
--
appendIfCompatible :: [Value] -> [Value] -> Either BError Value
appendIfCompatible [] ys = Right $ VList ys
appendIfCompatible xs [] = Right $ VList xs
appendIfCompatible xs@(x:_) ys@(y:_)
    | sameConstructor x y = Right . VList $ xs ++ ys  -- Fixed: Removed `pure`
    | otherwise           = Left $ ProgramError ExpectedList

-- | Extracts the list elements if the input is a homogeneous list.
-- Returns an error if not a list or not homogeneous.
--
-- == Examples:
--
-- >>> extract (VList [VInt 1, VInt 1])
-- Right [1,1]
--
-- >>> extract (VList [VInt 1, VBool True])
-- Left (ProgramError ExpectedList)
--
-- >>> extract (VList [])
-- Right []
--
extract :: Value -> Either BError [Value]
extract val = case validateElements val of
      Just (VList xs) -> Right xs
      _               -> Left $ ProgramError ExpectedList

-- | Applies a function to a validated homogeneous Either BError Valuelist.
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
    guard (all (sameConstructor x) rest)
    Just (VList xs)
validateElements v@(VList []) = Just v
validateElements _ = Nothing