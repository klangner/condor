{- |
Module : Condor.Statistics
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module counts different statistics on the text

-}
module Condor.Statistics 
    ( countWords
    ) where

import qualified Data.Text as T
import Condor.Text


-- | Count number of words in the text
countWords :: T.Text -> Int    
countWords = length . tokenize