module Doctest (main) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src"]
