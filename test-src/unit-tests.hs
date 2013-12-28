module Main where

import Condor.TextTest
import Test.HUnit


unittests :: Test
unittests = TestList Condor.TextTest.tests
 
main :: IO Counts
main = runTestTT unittests
