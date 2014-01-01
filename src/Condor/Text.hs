{- |
Module : Condor.Text
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Helper module with functions operating on Text.
Functions in this module are language neutral. 
Functions which are specific to the given language can be found in 
Condor.Language.<language> modules. 

-}
module Condor.Text 
    ( tokenize
    ) where
    
import Data.Char (toLower)


isSeparator :: Char -> Bool
isSeparator s = elem s " .,!?"

-- | Split string into tokens
tokenize :: String -> [String]
tokenize xs = case dropWhile isSeparator xs of
                   "" -> []
                   s' -> strToLower w : tokenize s''
                         where (w, s'') = break isSeparator s'

-- | Convert string to lower case                         
strToLower :: String -> String
strToLower xs = map toLower xs               

