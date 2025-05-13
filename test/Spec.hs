module Main (main) where
{-
import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src"]
-}

import Test.Hspec
import Data.List (intercalate)
import Interpreter.State (State(..), initialStateWithStack)
import Interpreter.Eval (interpret)


-- | A helper function for writing test cases.
-- Takes an input string (the program to interpret) and the expected final stack,
-- then creates an Hspec test case that checks if the interpreter produces the 
-- expected result.
t :: String -> String -> Spec
t input expected = it input $ do
  let state = initialStateWithStack [] []  -- set up your initial state
  case interpret input state of
    Right result -> intercalate "," (map show (stack result)) `shouldBe` expected
    Left err     -> expectationFailure $ "Unexpected error: " ++ show err


-- | The main entry point for the test suite.
-- Uses Hspec to group and run all test cases under a descriptive label.
main :: IO ()
main = hspec $ do
  describe "official tests for non-error programs" $ do
    {-- literals -}
    t "3"                           "3"    
    t "121231324135634563456363567" "121231324135634563456363567"
    t "1.0"                         "1.0"
    t "0.0"                         "0.0"
    t "-1"                          "-1"
    t "-1.1"                        "-1.1"
    t "False"                       "False"
    t "True"                        "True"
    t "[ [ ] [ ] ]"                 "[[],[]]"
    t "[ False [ ] True [ 1 2 ] ]"  "[False,[],True,[1,2]]"
    t "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""
    {-- quotation literals -}
    t "{ 20 10 + }"             "{ 20 10 + }"
    t "[ { + } { 10 + } { 20 10 + } ]"   "[{ + },{ 10 + },{ 20 10 + }]"
    
    {-- simple arithmetic -}
    t "1 1 +"               "2"       
    t "10 20 *"             "200"
    t "20 2 div"            "10"
    t "20 2 /"              "10.0"
    
    {-- arithmetic with type coercion -}
    t "1 1.0 +"             "2.0"       
    t "10 20.0 *"           "200.0"
    t "20 2.0 div"          "10"
    t "20.0 2.0 div"        "10"
    
    {-- bool operations -}
    t "False False &&"      "False"
    t "False True ||"       "True"
    t "False not"           "True"
    t "True not"            "False"
    
    {-- comparisons -}
    t "20 10 <"             "False"
    t "20 10 >"             "True"
    t "20 10.0 >"           "True"
    t "20.0 20.0 >"         "False"
    t "10 10 =="            "True"
    t "10 10.0 =="          "True"
    t "True True =="        "True"
    t "True 40 40 == =="    "True"
    t "\" abba \" \" abba \" ==" "True"
    t "[ ] [ ] =="          "True"
    t "[ 1 2 ] [ 1 2 ] =="  "True"
    t " [ [ ] ] [ [ ] ] ==" "True"
    
    {-- stack operations -}
    t "10 20 swap pop"          "20"
    t "10 dup dup + swap pop"   "20"
    t "10 20 swap dup + div"    "1"
    
    {-- length -}
    t "\" hello \" length"              "5"
    t "\" hello world \" length"        "11"
    t "[ 1 2 3 [ ] ] length"            "4"
    t "{ 10 20 + } length"              "3"

    {-- String parsing -}
    t "\" 12 \" parseInteger"           "12"
    t "\" 12.34 \" parseFloat"          "12.34"
    t "\" adam bob charlie \" words"    "[\"adam\",\"bob\",\"charlie\"]"          
    
    {-- lists -}
    t "[ 1 2 3 ]"           "[1,2,3]"
    t "[ 1 \" bob \" ]"     "[1,\"bob\"]"
    t "[ 1 2 ] empty"       "False"
    t "[ ] empty"           "True"
    t "[ 1 2 3 ] head"      "1"
    t "[ 1 2 3 ] length"    "3"
    t "[ 1 2 3 ] tail"      "[2,3]"
    t "1 [ ] cons"          "[1]"
    t "1 [ 2 3 ] cons"      "[1,2,3]"
    t "[ 1 ] [ 2 3 ] append" "[1,2,3]"
    t "[ 1 2 ] [ ] append"  "[1,2]"
    t "[ 1 ] [ 2 3 ] cons"  "[[1],2,3]"

    {-- list quotations -}
    t "[ 1 2 3 ] map { 10 * }"                              "[10,20,30]"
    t "[ 1 2 3 ] map { 1 + }"                               "[2,3,4]"
    t "[ 1 2 3 4 ] map { dup 2 > if { 10 * } { 2 * } }"     "[2,4,30,40]"
    t "[ 1 2 3 4 ] each { 10 * } + + +"                     "100"
    t "[ 1 2 3 4 ] 0 foldl { + }"                           "10"
    t "[ 2 5 ] 20 foldl { div }"                            "2"
    {-- note no { } needed for 1 instruction code -}
    t "[ \" 1 \" \" 2 \" \" 3 \" ] each { parseInteger } [ ] cons cons cons" "[1,2,3]"
    t "[ \" 1 \" \" 2 \" \" 3 \" ] each parseInteger [ ] 3 times cons"       "[1,2,3]"
    t "[ 1 2 3 4 ] 0 foldl +"                               "10"
    t "[ 2 5 ] 20 foldl div"                                "2"
    
    {-- assignments -}
    t "age"                             "age"
    t "age 10 := age"                   "10"
    t "10 age swap := age"              "10"
    t "[ 1 2 3 ] list swap := list"     "[1,2,3]"
    t "age 20 := [ 10 age ]"            "[10,20]"

    t "inc { 1 + } fun 1 inc"           "2"
    t "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" "110"
    
    {-- quotations -}
    t "{ 20 10 + } exec"                "30"
    t "10 { 20 + } exec"                "30"
    t "10 20 { + } exec"                "30"
    t "{ { 10 20 + } exec } exec"       "30"
    t "{ { 10 20 + } exec 20 + } exec"  "50"
    
    {-- if -}
    t "True if { 20 } { }"               "20"
    t "True if { 20 10 + } { 3 }"        "30"
    t "10 5 5 == if { 10 + } { 100 + }"  "20"
    t "False if { } { 45 }"              "45"
    t "True if { False if { 50 } { 100 } } { 30 }" "100"

    {-- if without quotation, more ergonomic expressions -}
    t "True if 20 { }"                 "20"
    t "True if { 20 10 + } 3"          "30"
    t "10 10 5 5 == if + { 100 + }"    "20"
    t "False if { } 45"                "45"
    t "True if { False if 50 100 } 30" "100"

    {-- times -}
    t "1 times { 100 50 + }"                               "150"
    t "5 times { 1 } [ ] 5 times { cons } 0 foldl { + }"   "5"
    t "5 times 1     [ ] 5 times   cons   0 foldl   +  "   "5"
    t "5 times { 10 } + + + +"                             "50"
    t "5 times 10 4 times +"                               "50"

    {-- loop -}
    t "1 loop { dup 4 > } { dup 1 + } [ ] 5 times { cons }"         "[1,2,3,4,5]"
    t "1 loop { dup 4 > } { dup 1 + } [ ] 5 times   cons  "         "[1,2,3,4,5]"
    t "[ 1 ] loop { dup length 9 > }  { dup head 1 + swap cons }"   "[10,9,8,7,6,5,4,3,2,1]"


    t "odd { dup 2 div swap 2 / == if False True } fun \
     \ 2 odd"                                                        "False"
    
    t "odd { dup 2 div swap 2 / == if False True } fun \
     \ 3 odd"                                                        "True"
    
    t "toList { [ ] swap times cons } fun \
     \ 1 2 3 4 \
     \ 4 toList"                                                      "[1,2,3,4]"
    
    t "gen1toNum { max swap := 1 loop { dup max > } { dup 1 + } } fun \
     \ 3 gen1toNum + + +"                                            "10"

    t "odd { dup 2 div swap 2 / == if False True } fun \
     \ toList { [ ] swap times cons } fun \
     \ gen1toNum { max swap := 1 loop { dup max > } { dup 1 + } } fun \
     \ 4 gen1toNum 5 toList map odd"                            "[True,False,True,False,True]"

