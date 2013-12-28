module Condor.TextTest (tests) where

import Test.HUnit
import Condor.Text


tests :: [Test]
tests = [TestLabel "" $ TestCase (prop_get_word_count "one two three" 3), 
         TestLabel "" $ TestCase (prop_get_word_count "one.two" 2)
        ]
         
prop_get_word_count :: String -> Int -> Assertion         
prop_get_word_count xs n = assertEqual xs (length (getWords xs)) n          