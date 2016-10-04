{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Condor.Core.IndexTest
Copyright : Copyright (C) 2013-2016 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable
-}

module Condor.Core.IndexSpec (spec) where

import Prelude
import Data.Text hiding(map)
import Data.Binary
import Condor.Core.Document (mkDocument)
import Condor.Core.Index
import Test.Hspec


spec :: Spec
spec = do

    describe "Index" $ do
        it "empty" $ (termCount emptyIndex)  `shouldBe` 0
        it "different words" $ countTermsHelper [("doc1", "one two three")] `shouldBe` 3
        it "Check stop words" $ countTermsHelper [("doc1", "one and two or three")] `shouldBe` 3
        it "2 documents" $ countDocsHelper [("doc1",  "one two three"), ("doc2", "forty two")] `shouldBe` 2

    describe "Search" $ do
        it "Same case" $ search (indexHelper [("doc1", "one two three")]) "two" `shouldBe` ["doc1"]
        it "First term" $ search (indexHelper [("doc1", "one two three"), ("doc2", "forty two")]) "one" `shouldBe` ["doc1"]
        it "Second document" $ search (indexHelper [("doc1", "one two three"), ("doc2", "forty two")]) "two"
            `shouldContain` ["doc2"]
        it "Lower case term. Document has upper case" $ search (indexHelper [("doc1", "One Two Three")]) "two"
            `shouldBe` ["doc1"]
        it "Upper case term. Document has lower case" $ search (indexHelper [("doc1", "one two three")]) "Three"
            `shouldBe` ["doc1"]
        it "2 terms" $ search (indexHelper [("doc1", "one two three")]) "one Three" `shouldBe` ["doc1"]
        it"Single doc" $ search (indexHelper [("doc1", "one two three"), ("doc2", "forty two")]) "two one"
            `shouldBe` ["doc2", "doc1"]

    describe "Serialize" $ do
        it "encode.decode" $ do
            let idx = indexHelper [("doc1", "one two three"), ("doc2", "forty two")]
            let idx2 = decode (encode idx)
            termCount idx `shouldBe` termCount idx2


indexHelper :: [(Text, Text)] -> Index
indexHelper xs = indexFromDocs $ map (\(k, v) -> mkDocument k v) xs

countTermsHelper :: [(Text, Text)] -> Int
countTermsHelper xs = termCount (indexHelper xs)

countDocsHelper :: [(Text, Text)] -> Int
countDocsHelper xs = docCount (indexHelper xs)
