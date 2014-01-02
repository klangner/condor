# Condor

Condor is Haskell Information Retrieval (IR) library which currently consists of the following modules:
* Condor.Index - Indexing and searching text documents.

## Index API

Create empty index

```Haskell
import Condor.Index

let idx = empty
```


Add document to the index

```Haskell
let idx1 = add idx "My document" "This is a document content."
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
