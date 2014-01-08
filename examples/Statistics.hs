module CountWords where

import Condor.Readers.Text
import Condor.NLP.Statistics
import Condor.Commons.DataTypes

countWordsExample :: IO Int
countWordsExample = do
    doc <- readDocument "haskell.txt"
    return $ countWords (docText doc)

