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
import Condor.NLP.Tokenizer
import Data.List


-- | Count number of words in the text.
--
-- > countWords (T.pack "one two three") == 3
countWords :: T.Text -> Int    
countWords = length . getWords . tokenize

-- | Count word frequency
--
-- > wordFreq (T.pack "one two, three one") == [("one", 2), ("two", 1), ("three", 1)]
wordFreq :: T.Text -> [(T.Text, Int)]
wordFreq a = [(head xs, length xs) | xs <- group tokens]
    where tokens = sort $ (foldCase . getWords . tokenize) a
