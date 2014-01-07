module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Condor.Index.MemoryTest (testCases)
import Condor.NLP.TextTest (testCases)
import Condor.NLP.StatisticsTest (testCases)
import Condor.NLP.English.PorterTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Condor.NLP.TextTest.testCases
                                      ++ Condor.NLP.English.PorterTest.testCases
                                      ++ Condor.NLP.StatisticsTest.testCases
                                      ++ Condor.Index.MemoryTest.testCases
                                      