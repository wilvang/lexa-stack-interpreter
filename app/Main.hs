{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Run.REPL (repl, startMessage)
import Interpreter.State ( State(..), initialStateWithStack)

main :: IO ()
main = getArgs >>= \case
  "r" : _   -> putStr startMessage >> repl (initialStateWithStack [] [])
  (f : _)   -> putStrLn "Running file"
  []        -> putStrLn "No args"