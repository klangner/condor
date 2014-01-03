{- |
Module : Condor.Index
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains functions which create, update and search index. 
Default implementation uses algorithms for english language (stemming, stop words etc.)
But it should be possible to customize it for any language by modifying IndexData data type.

Basic usage:
> import Consodr.Index
>
> index = add "My doc" "This is sample document content" empty
> search index "content"

-}
module Condor.Index 
    ( DocName
    , IndexData
    , Text
    , add
    , empty
    , search
    , size
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Binary
import Condor.Text
import Condor.Language.English.StopWords (isStopWord)
import Condor.Language.English.Porter (stem)


type DocName = String

type Text = String

-- | IndexData parameters. Those parameters can be used to change how
-- the text is processed before adding to the index
data IndexParams = IndexParams { ignore :: String -> Bool
                               , stemmer :: String -> String
                               }
                               
-- An instance of Binary to encode and decode an IndexParams in binary
instance Binary IndexParams where
     put _ = put (0 :: Word8)
     get = do tag <- getWord8
              case tag of
                  _ -> return $ IndexParams isStopWord stem                        

-- | Inverted index
data IndexData = IndexData { index :: Map.Map String [String]
                   , params :: IndexParams
                   }

-- An instance of Binary to encode and decode an IndexParams in binary
instance Binary IndexData where
     put i = do put (index i)
                put (params i)
     get = do i <- get
              p <- get
              return $ IndexData i p                        


-- | Create empty index. 
-- This index will be configured for english language.
empty :: IndexData
empty = IndexData Map.empty (IndexParams isStopWord stem)


-- | Add document to the index
add :: DocName -> Text -> IndexData -> IndexData
add d c ix = IndexData (foldl f (index ix) ws) (params ix)
    where ws = splitWords (params ix) c
          f i t = case Map.lookup t i of 
                    Just a -> Map.insert t (d:a) i
                    Nothing -> Map.insert t [d] i


-- | Search term in the index
search :: IndexData -> Text -> [DocName]
search ix s = List.nub $ foldl (++) [] ys
    where ys = map (searchTerm ix) ws
          ws = splitWords (params ix) s


-- | Search single term in the index
searchTerm :: IndexData -> Text -> [DocName]
searchTerm ix s = case Map.lookup s (index ix) of
                    Just a -> a
                    Nothing -> []
                 

-- | Get index size
size :: IndexData -> Int
size ix = Map.size (index ix)


-- | Split text into tokens.
-- This function removes stop words and stems words
splitWords :: IndexParams -> String -> [String]
splitWords p s = map (stemmer p) (filter f t)
    where t = tokenize s
          f = \x -> not ((ignore p) x)
        

