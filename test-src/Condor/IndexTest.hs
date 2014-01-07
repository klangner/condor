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
import Condor.Index
import Test.HUnit


testCases :: [(String, Test)]
testCases = [ ( "Empty index has length 0"
              , TestCase $ prop_empty)
            
            , ( "Space separates terms"
              , TestCase $ prop_termCount (T.pack "doc1", T.pack "one two three") 3)
              
            , ( "Check stop words"
              , TestCase $ prop_termCount (T.pack "doc1", T.pack "one and two or three") 3)
              
            , ( "Search for term with the same case"
               , TestCase $ prop_search "two" (T.pack "doc1") [ (T.pack "doc1"
                                                              , T.pack "one two three")])
               
            , ( "Search for first term"
              , TestCase $ prop_search "one" (T.pack "doc1") [ (T.pack "doc1", T.pack "one two three") 
                                                             , (T.pack "doc2", T.pack "forty two")
                                                             ])
                                       
            , ( "Search for term in second document"
              , TestCase $ prop_search "two" (T.pack "doc2") [ (T.pack "doc1", T.pack "one two three") 
                                                             , (T.pack "doc2", T.pack "forty two")
                                                             ])
                                                             
            , ( "Search for lower case term. Document has upper case"
              , TestCase $ prop_search "two" (T.pack "doc1") [(T.pack "doc1", T.pack "One Two Three")])
              
            , ( "Search for upperr case term. Document has lower case"
              , TestCase $ prop_search "Three" (T.pack "doc1") [(T.pack "doc1", T.pack "one two three")])
              
            , ( "Search for 2 terms"
              , TestCase $ prop_search "one Three" (T.pack "doc1") [(T.pack "doc1", 
                                                                     T.pack "one two three")])
               
            , ( "Single doc"
              , TestCase $ prop_search_count "one" 1 [ (T.pack "doc1", T.pack "one two three") 
                                                     , (T.pack "doc2", T.pack "forty two")
                                                     ])
                                                                     
            , ( "Document should be returned only once"
              , TestCase $ prop_search_count "two one" 2 [ (T.pack "doc1", T.pack "one two three") 
                                                         , (T.pack "doc2", T.pack "forty two")
                                                         ])
                                                         
            , ( "encode . decode == id"
              , TestCase $ prop_serialize [ (T.pack "doc1", T.pack "one two three") 
                                          , (T.pack "doc2", T.pack "forty two")
                                          ])                                                   
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
prop_termCount (d, c) n = assertEqual ("Index size: " ++ show c) 
                                      n 
                                      (termCount $ addDocument d c emptyIndex)         
    
-- | Check empty index         
prop_serialize :: [(DocName, DocContent)] -> Assertion         
prop_serialize ds = assertEqual "Serialize" (termCount idx) (termCount idx') 
    where idx = indexFromDocs ds           
          idx' = decode (encode idx)
         
    