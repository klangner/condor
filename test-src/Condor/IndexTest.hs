module Condor.TextTest (tests) where

import Condor.Index
import Test.HUnit
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [ ("Create index", TestCase $ prop_empty)
            ]
         
prop_empty :: String -> Int -> Assertion         
prop_empty = assertEqual "Empty index has 0 size" 0 (size empty)          
