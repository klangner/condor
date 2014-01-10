# Natural Language Processing

Using NLP modules it is possible to:

### Count number of words in the document

```haskell
import Condor.Reader.Text
import Condor.NLP.Statistics
import Condor.Commons.Document

doc <- readDocument "samples/haskell.txt"
countWords (docText doc)
```

### Get word frequency in the document

```haskell
import Data.Text
import Condor.Reader.Text
import Condor.NLP.Statistics
import Condor.Commons.Document

countWordsExample :: IO Int
countWordsExample = do
    doc <- readDocument "haskell.txt"
    return $ countWords (docText doc)

wordFreqExample :: IO [(Text, Int)]
wordFreqExample = do
    doc <- readDocument "haskell.txt"
    return $ wordFreq (docText doc)
```


