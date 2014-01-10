module CountWords where

import Data.Text
import Condor.Reader.Text(readDocument)
import Condor.NLP.Statistics(countWords, wordFreq)
import Condor.Commons.Document

countWordsExample :: IO Int
countWordsExample = do
    doc <- readDocument "haskell.txt"
    return $ countWords (docText doc)

wordFreqExample :: IO [(Text, Int)]
wordFreqExample = do
    doc <- readDocument "haskell.txt"
    return $ wordFreq (docText doc)
