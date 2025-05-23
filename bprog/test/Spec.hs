{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Parser (processTokens)
import Types (Result(..))

-- | Convert a Result to a String representation for comparison.
resultToString :: Result -> Either String String
resultToString (Success [v]) = Right (show v)
resultToString (Success vs)  = Right (show vs)
resultToString (Error _ msg) = Left ("Error: " ++ msg)
resultToString (Warning _ msg) = Left ("Warning: " ++ msg)
resultToString (Info _ msg) = Right msg

-- | Helper test runner for individual bprog programs.
t :: String -> String -> Spec
t input expected =
    it input $
        case resultToString (processTokens (words input) []) of
            Right actual -> actual `shouldBe` expected
            Left err     -> expectationFailure err

-- | Main test suite
main :: IO ()
main = hspec $ describe "Official bprog tests" $ do

    -- Literals
    t "3" "3"
    t "121231324135634563456363567" "121231324135634563456363567"
    t "1.0" "1.0"
    t "0.0" "0.0"
    t "-1" "-1"
    t "-1.1" "-1.1"
    t "False" "False"
    t "True" "True"
    t "[ [ ] [ ] ]" "[[],[]]"
    t "[ False [ ] True [ 1 2 ] ]" "[False,[],True,[1,2]]"
    t "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""

    -- Quotation literals
    t "{ 20 10 + }" "{ 20 10 + }"
    t "[ { + } { 10 + } { 20 10 + } ]" "[{ + },{ 10 + },{ 20 10 + }]"

    -- Arithmetic
    t "1 1 +" "2"
    t "10 20 *" "200"
    t "20 2 div" "10"
    t "20 2 /" "10.0"

    -- Arithmetic with type coercion
    t "1 1.0 +" "2.0"
    t "10 20.0 *" "200.0"
    t "20 2.0 div" "10"
    t "20.0 2.0 div" "10"

    -- Bool operations
    t "False False and" "False"
    t "False True or" "True"
    t "False not" "True"
    t "True not" "False"

    -- Comparisons
    t "20 10 <" "False"
    t "20 10 >" "True"
    t "20 10.0 >" "True"
    t "20.0 20.0 >" "False"
    t "10 10 ==" "True"
    t "10 10.0 ==" "True"
    t "True True ==" "True"
    t "True 40 40 == ==" "True"
    t "\" abba \" \" abba \" ==" "True"
    t "[ ] [ ] ==" "True"
    t "[ 1 2 ] [ 1 2 ] ==" "True"
    t "[ [ ] ] [ [ ] ] ==" "True"

    -- Stack operations
    t "10 20 swap pop" "20"
    t "10 dup dup + swap pop" "20"
    t "10 20 swap dup + div" "1"