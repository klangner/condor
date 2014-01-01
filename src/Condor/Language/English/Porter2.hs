{- |
Module : Condor.Language.English.Porter2
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains english (Porter2) stemming algorithm 
http://snowball.tartarus.org/algorithms/english/stemmer.html

-}
module Condor.Language.English.Porter2 (stem) where
    
import Data.List
 
    
-- | Stemming based on Porter2 algorithm
stem :: String -> String
stem s | length s < 3 = s
       | otherwise    = allSteps s

allSteps :: String -> String 
allSteps = step1 . step0


-- | Remove the longest among the suffixes
step0 :: String -> String
step0 s = takeWhile (/= '\'') s


-- | step1a + step1b
step1 :: String -> String
step1 = step1a

-- | Process suffixes
step1a :: String -> String
step1a s | endsWith s "sses" = take ((length s) - 2) s
         | or [endsWith s "ied", endsWith s "ies"] = 
                if length s >  4 then take ((length s) - 2) s
                else take ((length s) - 1) s
         | endsWith s "ss" = s
         | endsWith s "us" = s 
         | rule3 s = take ((length s) - 1) s
         | otherwise = s
         where rule3 x = and [ endsWith s "s"
                             , union (take ((length s) - 2) x) vowels /= []
                             ]
    
    
-- | Check if string ends with the given suffix
endsWith :: String -> String -> Bool
endsWith (x:xs) s | length (x:xs) < length s = False
                  | length (x:xs) == length s = (x:xs) == s
                  | otherwise = endsWith xs s
                  

-- | English vowels
vowels :: String
vowels = "aeiouy"                  