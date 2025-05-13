module Interpreter.Builtins.StringParsing (safeWords, safeIntParse, safeFloatParse) where

import Text.Read (readMaybe)
import Interpreter.Types (Value(..))
import Interpreter.Error (BError(..), ProgramError (..))

-- | Splits a 'VString' into a 'VList' of individual words.
--
-- Uses Haskell's 'words' function under the hood.
-- Returns an error if the input is not a 'VString'.
--
-- == Examples
--
-- >>> safeWords (VString "hello world test")
-- Right ["hello","world","test"]
--
-- >>> safeWords (VString "test")
-- Right ["test"]
--
-- >>> safeWords (VInt 42)
-- Left (ProgramError ExpectedString)
--
safeWords :: Value -> Either BError Value
safeWords (VString s) = Right . VList . map VString $ words s
safeWords _ = Left $ ProgramError ExpectedString

-- | Parses a 'VString' into a 'VInt' value.
--
-- Returns an error if parsing fails or input is not a string.
--
-- == Examples
--
-- >>> safeIntParse (VString "123")
-- Right 123
--
-- >>> safeIntParse (VString "hello")
-- Left (ProgramError NumberConversionError)
--
-- >>> safeIntParse (VFloat 3.14)
-- Left (ProgramError ExpectedString)
--
safeIntParse :: Value -> Either BError Value
safeIntParse = parseStringValue VInt

-- | Parses a 'VString' into a 'VFloat' value.
--
-- Returns an error if parsing fails or input is not a string.
--
-- == Examples
--
-- >>> safeFloatParse (VString "3.14")
-- Right 3.14
--
-- >>> safeFloatParse (VString "abc")
-- Left (ProgramError NumberConversionError)
--
-- >>> safeFloatParse (VBool True)
-- Left (ProgramError ExpectedString)
--
safeFloatParse :: Value -> Either BError Value
safeFloatParse = parseStringValue VFloat

-- | Attempts to parse a 'VString' into a typed 'Value' using the given constructor.
--
-- Used internally by 'safeIntParse' and 'safeFloatParse'.
-- Returns a 'ProgramError' if parsing fails or the input is not a 'VString'.
--
-- == Examples
--
-- >>> parseStringValue VInt (VString "42")
-- Right 42
--
-- >>> parseStringValue VFloat (VString "invalid")
-- Left (ProgramError NumberConversionError)
--
-- >>> parseStringValue VFloat (VBool True)
-- Left (ProgramError ExpectedString)
--
parseStringValue :: Read a => (a -> Value) -> Value -> Either BError Value
parseStringValue ctor (VString s) =
  case readMaybe s of
    Just x  -> Right (ctor x)
    Nothing -> Left $ ProgramError NumberConversionError
parseStringValue _ _ = Left $ ProgramError ExpectedString