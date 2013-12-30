module Condor.Index 
    ( empty
    , insert
    , search
    , size
    ) where

import qualified Data.Map as Map


type DocName = String
type Text = String
type Index = Map.Map String [String]

-- | Create empty index
empty :: Index
empty = Map.empty

-- | Add document to the index
insert :: Index -> DocName -> Text -> Index
insert idx _ _ = idx

-- | search term in the index
search :: Text -> [DocName]
search [] = []
search s = [s]

-- | Get index size
size :: Index -> Int
size ix = Map.size ix