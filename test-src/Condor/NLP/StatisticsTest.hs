{- |
Module : Condor.NLP.IndexTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Unit tests for Condor.Index module
-}

module Condor.NLP.StatisticsTest (testCases) where

import qualified Data.Text as T
import Condor.NLP.Statistics
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Count words"
              , TestCase $ prop_countWords "one two three" 3)
            
            ]
         
-- | Count number of returned documents          
prop_countWords :: String -> Int ->  Assertion         
prop_countWords xs n = assertEqual xs n $ countWords (T.pack xs)
