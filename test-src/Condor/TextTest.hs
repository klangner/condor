{- |
Module : Condor.TextTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Unit tests for Condor.Text module
-}

module Condor.TextTest (testCases) where

import Condor.Text
import Test.HUnit

testCases :: [(String, Test)]
testCases = [("Text", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_tokenize_count "one two three" 3
        , TestCase $ prop_tokenize_count "one,two " 2
        , TestCase $ prop_tokenize_token " one?two! " 1 "two"
        ]
         
prop_tokenize_count :: String -> Int -> Assertion         
prop_tokenize_count xs n = assertEqual xs n (length (tokenize xs))          

prop_tokenize_token :: String -> Int -> String -> Assertion         
prop_tokenize_token xs i t = assertEqual xs t ((tokenize xs)!!i)          