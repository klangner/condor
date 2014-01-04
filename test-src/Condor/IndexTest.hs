{- |
Module : Condor.IndexTest
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Unit tests for Condor.Index module
-}

module Condor.IndexTest (testCases) where

import Data.Binary
import Condor.Index
import Test.HUnit


testCases :: [(String, Test)]
testCases = [("Index", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_empty
        , TestCase $ prop_termCount ("doc1", "one two three") 3
        , TestCase $ prop_termCount ("doc1", "one and two or three") 3
        , TestCase $ prop_search "two" "doc1" [("doc1", "one two three")]
        , TestCase $ prop_search "one" "doc1" [ ("doc1", "one two three") 
                                              , ("doc2", "forty two")
                                              ]
        , TestCase $ prop_search "two" "doc2" [ ("doc1", "one two three") 
                                              , ("doc2", "forty two")
                                              ]
        , TestCase $ prop_search "two" "doc1" [("doc1", "One Two Three")]
        , TestCase $ prop_search "Three" "doc1" [("doc1", "one two three")]
        , TestCase $ prop_search "one Three" "doc1" [("doc1", "one two three")]
        , TestCase $ prop_search_count "two one" 2 [ ("doc1", "one two three") 
                                                   , ("doc2", "forty two")
                                                   ]
        , TestCase $ prop_serialize [ ("doc1", "one two three") 
                                    , ("doc2", "forty two")
                                    ]                                                   
        ]
         
-- | Helper function to populate index         
indexFromDocs :: [(DocName, DocContent)] -> Index
indexFromDocs ds = foldl f emptyIndex ds
    where f i (d, c) = addDocument d c i

         
-- | Check empty index         
prop_empty :: Assertion         
prop_empty = assertEqual "Empty index has 0 size" 0 (termCount emptyIndex)          
         
-- | Check if document is found         
prop_search :: String -> DocName -> [(DocName, DocContent)] ->  Assertion         
prop_search s e ds = assertEqual s True $ elem e (search idx s)
    where idx = indexFromDocs ds

-- | Count number of returned documents          
prop_search_count :: String -> Int -> [(DocName, DocContent)] ->  Assertion         
prop_search_count s n ds = assertEqual s n $ length (search idx s)
    where idx = indexFromDocs ds
         
-- | Check number of terms         
prop_termCount :: (DocName, DocContent) -> Int -> Assertion
prop_termCount (d, c) n = assertEqual ("Index size: " ++ c) n (termCount $ addDocument d c emptyIndex)         
    
-- | Check empty index         
prop_serialize :: [(DocName, DocContent)] -> Assertion         
prop_serialize ds = assertEqual "Serialize" (termCount idx) (termCount idx') 
    where idx = indexFromDocs ds           
          idx' = decode (encode idx)
         
    