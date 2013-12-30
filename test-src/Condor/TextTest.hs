module Condor.TextTest (tests) where

import Condor.Text
import Test.HUnit
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [ ("Get word count", TestCase $ prop_tokenize_count "one two three" 3)
            , ("Get word count", TestCase $ prop_tokenize_count "one,two " 2)
            , ("Get word count", TestCase $ prop_tokenize_token " one?two! " 1 "two")
            ]
         
prop_tokenize_count :: String -> Int -> Assertion         
prop_tokenize_count xs n = assertEqual xs n (length (tokenize xs))          

prop_tokenize_token :: String -> Int -> String -> Assertion         
prop_tokenize_token xs i t = assertEqual xs t ((tokenize xs)!!i)          