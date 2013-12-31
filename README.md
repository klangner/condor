# Condor

Condor is Haskell library (and command line tool) to index and search text documents.

## Library API

Create empty index

```Haskell
import Condor.Index

let idx = empty
```


Add document to the index

```Haskell
let idx1 = add idx "My document" "This is document content."
```


Count number of entries

```Haskell
size idx1
4
```


Search documents

```Haskell
search idx1 "content"
["My document"]
```
