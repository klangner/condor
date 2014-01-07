{- |
Module : Condor.NLP.Text
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
module Condor.NLP.Text 
    ( tokenize
    ) where
    
import qualified Data.Text as T


-- | List of separators
isSeparator :: Char -> Bool
isSeparator s = elem s " .,!?"

-- | Remove suffix separator 
removeSeparators:: T.Text -> T.Text
removeSeparators = T.dropWhile isSeparator


-- | Split string into tokens
tokenize :: T.Text -> [T.Text]
tokenize xs = if T.length s == 0 then []
              else (T.toCaseFold t : tokenize r)
                where s = removeSeparators xs
                      (t, r) = T.break isSeparator (removeSeparators xs)
