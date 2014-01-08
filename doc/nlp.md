# Natural Language Processing

## Counting number of words in the document

Try this in ghci session:
```haskell
import Condor.Readers.Text
import Condor.NLP.Statistics
import Condor.Commons.DataTypes

doc <- readDocument "samples/haskell.txt"
countWords (docText doc)
```


