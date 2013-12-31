module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Condor.TextTest (testCases)
import Condor.IndexTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Condor.TextTest.testCases
                                      ++ Condor.IndexTest.testCases
