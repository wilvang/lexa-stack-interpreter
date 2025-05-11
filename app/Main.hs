{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Run.REPL (repl, startMessage)
import Run.File (runFile)
import Interpreter.State (initialStateWithStack)

main :: IO ()
main = getArgs >>= \case
  "r" : _   -> putStr startMessage >> repl (initialStateWithStack [] [])
  (f : _)   -> runFile f (initialStateWithStack [] [])
  []        -> putStrLn "No args"