{- |
Module : Condor.NLP.Statistics
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module counts different statistics on the text

-}
module Condor.NLP.Statistics 
    ( countWords
    , wordFreq
    ) where

import qualified Data.Text as T
import Condor.NLP.Text


-- | Count number of words in the text.
-- 
-- Example:
--
-- > countWords (T.pack "This is word counter")
-- > 4
countWords :: T.Text -> Int    
countWords = length . tokenize

-- | Count word frequency
-- 
-- Example:
--
-- > wordFreq (T.pack "one two three one")
-- > [("one",2), ("two", 1), ("three", 1)]
wordFreq :: T.Text -> [(T.Text, Int)]
wordFreq a = [(a,1)]