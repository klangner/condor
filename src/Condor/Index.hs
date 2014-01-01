{- |
Module : Condor.Index
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains functions which create, update and search index. 
Default implementation uses algorithms for english language (stemming, stop words etc.)
But it should be possible to customize it for any language by modifying Index data type.

Basic usage:
> import Consodr.Index
>
> index = add "My doc" "This is sample document content" empty
> search index "content"

-}
module Condor.Index 
    ( DocName
    , Index
    , Text
    , add
    , empty
    , search
    , size
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Condor.Text
import qualified Condor.Language.English as English


type DocName = String

type Text = String

data IndexParams = IndexParams { ignore :: String -> Bool
                               , stemmer :: String -> String
                               }

data Index = Index { index :: Map.Map String [String]
                   , params :: IndexParams
                   }

-- | Create empty index. 
-- This index will be configured for english language.
empty :: Index
empty = Index Map.empty (IndexParams English.isStopWord English.stem)


-- | Add document to the index
add :: DocName -> Text -> Index -> Index
add d c ix = Index (foldl f (index ix) ws) (params ix)
    where ws = splitWords (params ix) c
          f i t = case Map.lookup t i of 
                    Just a -> Map.insert t (d:a) i
                    Nothing -> Map.insert t [d] i


-- | Search term in the index
search :: Index -> Text -> [DocName]
search ix s = List.nub $ foldl (++) [] ys
    where ys = map (searchTerm ix) ws
          ws = splitWords (params ix) s


-- | Search single term in the index
searchTerm :: Index -> Text -> [DocName]
searchTerm ix s = case Map.lookup s (index ix) of
                    Just a -> a
                    Nothing -> []
                 

-- | Get index size
size :: Index -> Int
size ix = Map.size (index ix)


-- | Split text into tokens.
-- This function removes stop words and stems words
splitWords :: IndexParams -> String -> [String]
splitWords p s = filter f t
    where t = map (stemmer p) (tokenize s)
          f = \x -> not ((ignore p) x)
        

