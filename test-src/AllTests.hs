module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Condor.TextTest (testCases)
import Condor.IndexTest (testCases)
import Condor.Language.English.PorterTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Condor.TextTest.testCases
                                      ++ Condor.IndexTest.testCases
                                      ++ Condor.Language.English.PorterTest.testCases
