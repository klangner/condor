# Index API

This library for performance reasons is based on Data.Text (unicode strings).

Create empty index

```Haskell
import Condor.Search.Index
import Condor.Commons.Document

let idx = emptyIndex
```


Add document to the index

```Haskell
let idx1 = addDocument idx $ docFromStrings "Document name" "This is a document content."
```


Count the number of entries

```Haskell
termCount idx1
2
```


Search for documents

```Haskell
search idx1 "content"
["My document"]
```


This is alpha version of the library. This means that the API can change in next releases.
