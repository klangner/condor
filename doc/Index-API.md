# Index API

Create empty index

```Haskell
import Condor.Index

let idx = emptyIndex
```


Add document to the index

```Haskell
let idx1 = addDocument idx "My document" "This is a document content."
```


Count the number of entries

```Haskell
size idx1
5
```


Search for documents

```Haskell
search idx1 "content"
["My document"]
```


This is alpha version of the library. This means that the API can change in next releases.
