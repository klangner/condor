{- |
Module : Condor.NLP.Statistics
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module counts different statistics on the text

-}
module Condor.NLP.Statistics 
    ( countWords
    ) where

import qualified Data.Text as T
import Condor.NLP.Text


-- | Count number of words in the text
countWords :: T.Text -> Int    
countWords = length . tokenize