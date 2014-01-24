{- |
Module : Condor.Search.Index
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Memory based index.
This module contains functions which create, update and search index. 
Default implementation uses algorithms for english language (stemming, stop words etc.)

Functions in this module (for performance reasons) are based on unicode strings from module Data.Text.

Basic usage:

> import Condor.Search.Index (addDocument, search)
> import Condor.Commons.Document (docFromStrings)
>
> let idx = addDocument emptyIndex $ docFromStrings "My document 1" "This is a document content."
> search idx "content"
> ["My document 1"]

-}
module Condor.Search.Index 
    ( Index
    , Term
    , addDocument
    , addDocTerms
    , emptyIndex
    , docCount
    , search
    , searchTerms
    , termCount
    ) where

import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Binary

import Condor.Commons.Unsafe()
import Condor.Commons.Document (DocName, Document(..), docName, docText)
import Glider.NLP.Language.English.StopWords (isStopWord)
import Glider.NLP.Language.English.Porter (stem)
import Glider.NLP.Tokenizer


-- | Single term. Could be normalized word
type Term = Text

-- | Inverted index
data Index = Index { terms :: Map.Map Term [Int]
                   , docs :: [DocName]
                   }

-- | An instance of Binary to encode and decode an IndexParams in binary
instance Binary Index where
     put i = do 
            put (terms i)
            put (docs i)
     get = do i <- get
              d <- get
              return $ Index i d                        


-- | Create empty index. 
-- This index will be configured for english language.
emptyIndex :: Index
emptyIndex = Index Map.empty []


-- | Add document to the index.
-- This function uses algorithms for english language to split document content
-- into index terms.
addDocument :: Document -> Index -> Index
addDocument d = addDocTerms (docName d) (splitTerms content)
    where content = docText d


-- | Add document to the index.
-- This function should be used if document content should be splitted into terms
-- with custom algorithms.
addDocTerms :: DocName -> [Term] -> Index -> Index
addDocTerms d c ix = Index (foldl f (terms ix) c) (d:docs ix)
    where f ix' t = case Map.lookup t ix' of 
                    Just a -> Map.insert t (index:a) ix'
                    Nothing -> Map.insert t [index] ix'
          index = length (docs ix)


-- | Search terms given as single string in the index
-- This function uses algorithms for english language to split query into tokens.
search :: Index -> Text -> [DocName]
search ix s = searchTerms ix (splitTerms s)


-- | Search terms given as array in the index.
-- This function should be used if query should be splitted into terms
-- with custom algorithms
searchTerms :: Index -> [Term] -> [DocName]
searchTerms ix s = List.nub $ concat ys
    where ys = map (findDocs ix) s


-- | Search single term in the index
findDocs :: Index -> Term -> [DocName]
findDocs ix s = case Map.lookup s (terms ix) of
                    Just a -> map (reverse (docs ix) !!) a
                    Nothing -> []
                 

-- | Get the number of terms in the index
termCount :: Index -> Int
termCount ix = Map.size (terms ix)
                 

-- | Get the number of documents in the index
docCount :: Index -> Int
docCount ix = length (docs ix)


-- | Split text into terms.
-- This function removes stop words and stems words
splitTerms :: Text -> [Term]
splitTerms s = map stem (filter (not . isStopWord) t)
    where t = (foldCase . getWords . tokenize) s
        

