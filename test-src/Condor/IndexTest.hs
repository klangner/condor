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
import qualified Data.Text as T
import Condor.Definition (DocName, Document(..), docFromStrings)
import Condor.Index
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Empty index has length 0"
              , TestCase $ prop_empty)
            
            , ( "Space separates terms"
              , TestCase $ prop_termCount (docFromStrings "doc1" "one two three") 3)
              
            , ( "Check stop words"
              , TestCase $ prop_termCount (docFromStrings "doc1" "one and two or three") 3)
              
            , ( "Search for term with the same case"
               , TestCase $ prop_search "two" (T.pack "doc1") [docFromStrings "doc1" "one two three"])
               
            , ( "Search for first term"
              , TestCase $ prop_search "one" (T.pack "doc1") [ docFromStrings "doc1" "one two three" 
                                                             , docFromStrings "doc2" "forty two"
                                                             ])
                                       
            , ( "Search for term in second document"
              , TestCase $ prop_search "two" (T.pack "doc2") [ docFromStrings "doc1" "one two three" 
                                                             , docFromStrings "doc2" "forty two"
                                                             ])
                                                             
            , ( "Search for lower case term. Document has upper case"
              , TestCase $ prop_search "two" (T.pack "doc1") [docFromStrings "doc1" "One Two Three"])
              
            , ( "Search for upperr case term. Document has lower case"
              , TestCase $ prop_search "Three" (T.pack "doc1") [docFromStrings "doc1" "one two three"])
              
            , ( "Search for 2 terms"
              , TestCase $ prop_search "one Three" (T.pack "doc1") [docFromStrings "doc1" "one two three"])
               
            , ( "Single doc"
              , TestCase $ prop_search_count "one" 1 [ docFromStrings "doc1" "one two three" 
                                                     , docFromStrings "doc2" "forty two"
                                                     ])
                                                                     
            , ( "Document should be returned only once"
              , TestCase $ prop_search_count "two one" 2 [ docFromStrings "doc1" "one two three" 
                                                         , docFromStrings "doc2" "forty two"
                                                         ])
                                                         
            , ( "encode . decode == id"
              , TestCase $ prop_serialize [ docFromStrings "doc1" "one two three" 
                                          , docFromStrings "doc2" "forty two"
                                          ])                                                   
        ]
         
-- | Helper function to populate index         
indexFromDocs :: [Document] -> Index
indexFromDocs ds = foldl f emptyIndex ds
    where f i d = addDocument d i

         
-- | Check empty index         
prop_empty :: Assertion         
prop_empty = assertEqual "Empty index has 0 size" 0 (termCount emptyIndex)          
         
-- | Check if document is found         
prop_search :: String -> DocName -> [Document] ->  Assertion         
prop_search s e ds = assertEqual s True $ elem e (search idx s)
    where idx = indexFromDocs ds

-- | Count number of returned documents          
prop_search_count :: String -> Int -> [Document] ->  Assertion         
prop_search_count s n ds = assertEqual s n $ length (search idx s)
    where idx = indexFromDocs ds
         
-- | Check number of terms         
prop_termCount :: Document -> Int -> Assertion
prop_termCount d n = assertEqual "Count terms" n (termCount $ addDocument d emptyIndex)         
    
-- | Check empty index         
prop_serialize :: [Document] -> Assertion         
prop_serialize ds = assertEqual "Serialize" (termCount idx) (termCount idx') 
    where idx = indexFromDocs ds           
          idx' = decode (encode idx)
         
    