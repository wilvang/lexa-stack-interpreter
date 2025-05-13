{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Run.REPL (repl, startMessage)
import Run.File (runFile, importLib)

-- | The main entry point of the program.
--
-- This module defines the main function for the application, which handles
-- command-line arguments and runs the program accordingly. It can either
-- start the REPL (Read-Eval-Print Loop) or run a script file, depending on
-- the provided arguments. Additionally, it loads a standard library (`stdlib.lexa`)
-- to initialize the base state before running any program.
--
-- == Behavior:
--
-- * If the argument "-r" is provided, the program starts the REPL with a predefined
--   base state (which includes the standard library).
-- * If a filename is provided as an argument, the program loads and runs the file
--   with the given base state.
-- * If no arguments are provided, the program prints "No args was provided."
--
main :: IO ()
main = readFile "app/stdlib.lexa" >>= \libCode ->
    let baseState = importLib libCode in
    getArgs >>= \case
  "-r" : _   -> putStr startMessage >> repl baseState
  (f : _)   -> runFile f baseState
  []        -> putStrLn "No args was provided."