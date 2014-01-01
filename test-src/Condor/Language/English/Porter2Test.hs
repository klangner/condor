{- |
Module : Condor.Language.English.Porter2Test
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Unit tests for Condor.Language.English.Porter2 module
-}

module Condor.Language.English.Porter2Test (testCases) where

import Condor.Language.English.Porter2
import Test.HUnit


testCases :: [(String, Test)]
testCases = [("Porter2", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_stem "consign" "consign"
        , TestCase $ prop_stem "class's" "class"
        , TestCase $ prop_stem "classes" "class"
        , TestCase $ prop_stem "cried" "cri"
        , TestCase $ prop_stem "ties" "tie"
        --, TestCase $ prop_stem "gas" "gas"
        , TestCase $ prop_stem "gaps" "gap"
        ]
         
prop_stem :: String -> String -> Assertion         
prop_stem a b = assertEqual a b (stem a)          
