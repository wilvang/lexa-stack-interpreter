{-# LANGUAGE LambdaCase #-}
module Interpreter.Builtins.List (safeHead, safeTail, safeEmpty, safeLength, safeCons, safeAppend) where

import Interpreter.Types (Value(..), Token(..))
import Interpreter.Error (BError(ProgramError), ProgramError(..))

-- | Safely retrieves the head of a homogeneous list.
-- Returns an error if the list is empty or not homogeneous.
--
-- == Examples:
--
-- $setup
-- >>> import Interpreter.Types ( Op(..) )
--
-- >>> safeHead (VList [VInt 1, VInt 1, VInt 1])
-- Right 1
--
-- >>> safeHead (VList [])
-- Left (ProgramError EmptyList)
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
-- Right [1,1]
--
-- >>> safeTail (VList [VInt 42])
-- Right []
--
-- >>> safeTail (VList [])
-- Left (ProgramError EmptyList)
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
-- >>> safeLength (VString "Hello")
-- Right 5
--
-- >>> safeLength (VQuotation [TokOp OpAdd])
-- Right 1
--
safeLength :: Value -> Either BError Value
safeLength = (`withValidatedList` Right . VInt . toInteger . length)

-- | Safely prepends an element to a homogeneous list.
-- If the list is empty, any element is allowed.
-- If the list is non-empty, the new element must match the existing elements to preserve homogeneity.
-- Returns an error if the types/values are incompatible.
--
-- == Examples:
--
-- >>> safeCons (VInt 0) (VList [VInt 1, VInt 1])
-- Right [0,1,1]
--
-- >>> safeCons (VBool True) (VList [])
-- Right [True]
--
-- >>> safeCons (VString "a") (VList [VString "b", VString "c"])
-- Right ["a","b","c"]
--
-- >>> safeCons (VInt 1) (VInt 2)
-- Left (ProgramError ExpectedList)
--
safeCons :: Value -> Value -> Either BError Value
safeCons v lst = withValidatedList lst $ \case
  [] -> Right $ VList [v]
  xs -> Right $ VList (v:xs)

-- | Safely appends two homogeneous lists.
-- If either list is empty, it returns the non-empty list.
-- If both are non-empty, it checks that the elements are of the same type/value.
-- Returns an error if they are not compatible.
--
-- == Examples:
--
-- >>> safeAppend (VList [VInt 1, VInt 1]) (VList [VInt 1, VInt 1])
-- Right [1,1,1,1]
--
-- >>> safeAppend (VList []) (VList [VBool True])
-- Right [True]
--
-- >>> safeAppend (VList [VString "a"]) (VList [])
-- Right ["a"]
--
safeAppend :: Value -> Value -> Either BError Value
safeAppend v1 v2 = do
  xs <- toListLike v1
  ys <- toListLike v2
  let combined = xs ++ ys
  case (v1, v2) of
    (VString _, VString _) -> listToString (VList combined)  -- convert back to string
    (VList _, VList _)     -> Right $ VList combined
    _                      -> Left $ ProgramError ExpectedList

-- | Applies a function to a validated homogeneous Either BError Valuelist.
-- Returns an error if the input is not a valid homogeneous list.
--
-- == Examples:
--
-- >>> withValidatedList (VList [VInt 1, VInt 1]) (Right . VInt . toInteger . length)
-- Right 2
--
withValidatedList :: Value -> ([Value] -> Either BError Value) -> Either BError Value
withValidatedList val f = 
    case toListLike val of
        Right xs  -> f xs
        Left err  -> Left err

-- | Converts a list-like 'Value' (such as a 'VList', 'VString', or 'VQuotation') 
-- into a homogeneous list of 'Value's.
-- 
-- - For 'VList', it returns the inner list directly.
-- - For 'VString', it converts each character into a single-character 'VString'.
-- - For 'VQuotation', it wraps each token into a 'Value'. 'TokVal' is unwrapped directly,
--   while 'TokOp' is represented as a string using 'show'.
--
-- Returns an error if the value is not list-like.
--
-- == Examples:
--
-- >>> toListLike (VList [VInt 1, VInt 2])
-- Right [1,2]
--
-- >>> toListLike (VString "hi")
-- Right ["h","i"]
--
-- >>> toListLike (VQuotation [TokOp OpAdd])
-- >>> Right ["+"]
--
-- >>> toListLike (VInt 3)
-- Left (ProgramError ExpectedList)
--
toListLike :: Value -> Either BError [Value]
toListLike (VList xs)     = Right xs
toListLike (VString s)    = Right $ map (VString . (:[])) s
toListLike (VQuotation q) = Right $ map wrap q
  where
    wrap (TokVal v) = v
    wrap (TokOp op) = VString (show op) 
toListLike _              = Left $ ProgramError ExpectedList

-- | Converts a list of single-character strings back into a single 'VString'.
-- 
-- Returns an error if any element in the list is not a single-character 'VString'.
--
-- == Examples:
--
-- >>> listToString (VList [VString "a", VString "b"])
-- Right "ab"
--
-- >>> listToString (VList [VString "hello"])
-- Left (ProgramError ExpectedString)
--
-- >>> listToString (VInt 1)
-- Left (ProgramError ExpectedString)
--
listToString :: Value -> Either BError Value
listToString (VList v) = fmap VString (mapM extractChar v)
  where
    extractChar (VString [c]) = Right c
    extractChar _             = Left $ ProgramError ExpectedString
listToString _ = Left $ ProgramError ExpectedString