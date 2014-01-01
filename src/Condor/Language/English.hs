{- |
Module : Condor.Language.English
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Module contains functions specific to the english language

-}

module Condor.Language.English 
    ( isStopWord
    , stem
    ) where
    

-- | Stemming words
stem :: String -> String
stem s = s        

-- | Predicate to check if given words is a stop word
isStopWord :: String -> Bool
isStopWord _ = False
