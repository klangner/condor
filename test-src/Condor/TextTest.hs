module Condor.TextTest (tests) where

import Condor.Text
import Test.HUnit
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [ ("Get word count", TestCase $ prop_get_word_count "one two three" 3)
            , ("Get word count", TestCase $ prop_get_word_count "one two " 2)
            ]
         
prop_get_word_count :: String -> Int -> Assertion         
prop_get_word_count xs n = assertEqual xs n (length (getWords xs))          