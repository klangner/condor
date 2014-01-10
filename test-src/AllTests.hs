module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Condor.Search.IndexTest (testCases)
import Condor.NLP.TokenizerTest (testCases)
import Condor.NLP.StatisticsTest (testCases)
import Condor.NLP.Language.English.PorterTest (testCases)
import Condor.NLP.Language.DefaultTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Condor.NLP.TokenizerTest.testCases
                                      ++ Condor.NLP.Language.English.PorterTest.testCases
                                      ++ Condor.NLP.Language.DefaultTest.testCases
                                      ++ Condor.NLP.StatisticsTest.testCases
                                      ++ Condor.Search.IndexTest.testCases
                                      