{- |
Module : Condor.Language.English
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains english (Porter2) stemming algorithm 
http://snowball.tartarus.org/algorithms/english/stemmer.html

-}
module Condor.Language.English.Porter2 (stem) where
    
    
-- | Stemming based on Porter2 algorithm
stem :: String -> String
stem w = w
