module Condor.IndexTest (testCases) where

import Condor.Index
import Test.HUnit


testCases :: [(String, Test)]
testCases = [("Index", t) | t <- tests]

tests :: [Test]
tests = [ TestCase $ prop_empty
        , TestCase $ prop_size ("doc1", "one two three") 3
        , TestCase $ prop_search "two" "doc1" [("doc1", "one two three")]
        , TestCase $ prop_search "one" "doc1" [ ("doc1", "one two three") 
                                              , ("doc2", "forty two")
                                              ]
        , TestCase $ prop_search "two" "doc2" [ ("doc1", "one two three") 
                                              , ("doc2", "forty two")
                                              ]
        , TestCase $ prop_search "two" "doc1" [("doc1", "One Two Three")]
        , TestCase $ prop_search "Three" "doc1" [("doc1", "one two three")]
        ]
         
-- | Helper function to populate index         
indexFromDocs :: [(DocName, Text)] -> Index
indexFromDocs ds = foldl f empty ds
    where f i (d, c) = add d c i

         
prop_empty :: Assertion         
prop_empty = assertEqual "Empty index has 0 size" 0 (size empty)          
         
prop_search :: String -> DocName -> [(DocName, Text)] ->  Assertion         
prop_search s e ds = assertEqual s True $ elem e (search idx s)
    where idx = indexFromDocs ds
         
prop_size :: (DocName, Text) -> Int -> Assertion
prop_size (d, c) n = assertEqual "Index size" n (size $ add d c empty)         
    