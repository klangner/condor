{- |
Module : Condor.NLP.TextTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Condor.NLP.TextTest (testCases) where

import qualified Data.Text as T
import Condor.NLP.Text
import Test.HUnit


testCases :: [(String, Test)]
testCases = [("Text", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_tokenize_count (T.pack "one two three") 3
        , TestCase $ prop_tokenize_count (T.pack "one,two ") 2
        , TestCase $ prop_tokenize_token (T.pack " one?two! ") 1 (T.pack "two")
        ]
         
prop_tokenize_count :: T.Text -> Int -> Assertion         
prop_tokenize_count xs n = assertEqual (show xs) n (length (tokenize xs))          

prop_tokenize_token :: T.Text -> Int -> T.Text -> Assertion         
prop_tokenize_token xs i t = assertEqual (show xs) t ((tokenize xs)!!i)          