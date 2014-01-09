{- |
Module : Condor.NLP.Language.Default
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains default implementation for language specific algorithms.
If there is no specific algorithm for yor language you can use one from this module.

-}
module Condor.NLP.Language.Default (isStopWord) where
    
import qualified Data.Text as T
    

-- | Any less then 4 characters words is marked as stop word
isStopWord :: T.Text -> Bool
isStopWord w = T.length w < 4
