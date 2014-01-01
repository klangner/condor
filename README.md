# Condor

Condor is Haskell library for indexing and searching text documents.

## Library API

Create empty index

```Haskell
import Condor.Index

let idx = empty
```


Add document to the index

```Haskell
let idx1 = add idx "My document" "This is a document content."
```


Count number of entries

```Haskell
size idx1
5
```


Search documents

```Haskell
search idx1 "content"
["My document"]
```

This is alpha version of the library. This means that the API can change in next releases.
