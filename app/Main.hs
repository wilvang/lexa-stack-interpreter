{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Run.REPL (repl, startMessage)
import Run.File (runFile, importLib)

main :: IO ()
main = readFile "app/stdlib.lexa" >>= \libCode ->
    let baseState = importLib libCode in
    getArgs >>= \case
  "r" : _   -> putStr startMessage >> repl baseState
  (f : _)   -> runFile f baseState
  []        -> putStrLn "No args was provided."