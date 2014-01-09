# Condor

Condor is Haskell Information Retrieval (IR) library which can be used to index, search and analyze documents.

## Library overview

![Overview](https://raw.github.com/klangner/Condor/master/doc/overview.png)

Condor consists of the following packages:
* Search - For indexing and searching document
* Reader - For reading content from different file types
* NLP - For natural language processing functions

## Using Condor

* [Indexing and searching documents](doc/index.md)
* [Natural Language processing](doc/nlp.md)
* [Using Condor as command line tool](doc/Command-line-usage.md)


## Folders

* src - Contains library code
* src-test - Contains unit tests
* src-app - contains sources for command line application
* examples - Contain haskell examples which can be loaded in ghci and executed to test API
* samples - contains small datasets for testing purposes

This is alpha version of the library. It means that the API can change in the next releases.
