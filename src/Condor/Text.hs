module Condor.Text (
    tokenize
    ) where


isSeparator :: Char -> Bool
isSeparator s = elem s " .,!?"

-- | Split string into tokens
tokenize :: String -> [String]
tokenize xs = case dropWhile isSeparator xs of
                   "" -> []
                   s' -> w : tokenize s''
                         where (w, s'') = break isSeparator s'

