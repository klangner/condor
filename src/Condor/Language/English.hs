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
    
import Data.Set    
    

-- | Stemming words
stem :: String -> String
stem s = s        

-- | Predicate to check if given words is a stop word
isStopWord :: String -> Bool
isStopWord w = member w stopWords

stopWords :: Set String
stopWords = fromList [ "i"
                     , "me"
                     , "my"
                     , "myself"
                     , "we"
                     , "our"
                     , "ours"
                     , "ourselves"
                     , "you"
                     , "your"
                     , "yours"
                     , "yourself"
                     , "yourselves"
                     , "he"
                     , "him"
                     , "his"
                     , "himself"
                     , "she"
                     , "her"
                     , "hers"
                     , "herself"
                     , "it"
                     , "its"
                     , "itself"
                     , "they"
                     , "them"
                     , "their"
                     , "theirs"
                     , "themselves"
                     , "what"
                     , "which"
                     , "who"
                     , "whom"
                     , "this"
                     , "that"
                     , "these"
                     , "those"
                     , "am"
                     , "is"
                     , "are"
                     , "was"
                     , "were"
                     , "be"
                     , "been"
                     , "being"
                     , "have"
                     , "has"
                     , "had"
                     , "having"
                     , "do"
                     , "does"
                     , "did"
                     , "doing"
                     , "a"
                     , "an"
                     , "the"
                     , "and"
                     , "but"
                     , "if"
                     , "or"
                     , "because"
                     , "as"
                     , "until"
                     , "while"
                     , "of"
                     , "at"
                     , "by"
                     , "for"
                     , "with"
                     , "about"
                     , "against"
                     , "between"
                     , "into"
                     , "through"
                     , "during"
                     , "before"
                     , "after"
                     , "above"
                     , "below"
                     , "to"
                     , "from"
                     , "up"
                     , "down"
                     , "in"
                     , "out"
                     , "on"
                     , "off"
                     , "over"
                     , "under"
                     , "again"
                     , "further"
                     , "then"
                     , "once"
                     , "here"
                     , "there"
                     , "when"
                     , "where"
                     , "why"
                     , "how"
                     , "all"
                     , "any"
                     , "both"
                     , "each"
                     , "few"
                     , "more"
                     , "most"
                     , "other"
                     , "some"
                     , "such"
                     , "no"
                     , "nor"
                     , "not"
                     , "only"
                     , "own"
                     , "same"
                     , "so"
                     , "than"
                     , "too"
                     , "very"
                     , "s"
                     , "t"
                     , "can"
                     , "will"
                     , "just"
                     , "don"
                     , "should"
                     , "now"
                     ]

