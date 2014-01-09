module CountWords where

import Condor.Reader.Text
import Condor.NLP.Statistics(countWords)
import Condor.Commons.Document(readDocument)

countWordsExample :: IO Int
countWordsExample = do
    doc <- readDocument "haskell.txt"
    return $ countWords (docText doc)

