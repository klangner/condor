module Condor.Text 
    ( stem
    , tokenize
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

-- | Stemming words
stem :: String -> String
stem s = s        

