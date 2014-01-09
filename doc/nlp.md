# Natural Language Processing

## Counting number of words in the document

Try this in ghci session:
```haskell
import Condor.Reader.Text
import Condor.NLP.Statistics
import Condor.Commons.Document

doc <- readDocument "samples/haskell.txt"
countWords (docText doc)
```


