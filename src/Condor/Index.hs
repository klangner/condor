module Condor.Index 
    ( DocName
    , Text
    , add
    , empty
    , search
    , size
    ) where

import qualified Data.Map as Map
import Condor.Text (tokenize)


type DocName = String
type Text = String
data Index = Index { index :: Map.Map String [String]
                   }


-- | Create empty index
empty :: Index
empty = Index Map.empty

-- | Add document to the index
add :: DocName -> Text -> Index -> Index
add d c idx = Index $ foldl f (index idx) (tokenize c)
    where f ix t = Map.insert t [d] ix

-- | search term in the index
search :: Index -> Text -> [DocName]
search _ "" = []
search _ s = [s]

-- | Get index size
size :: Index -> Int
size ix = Map.size (index ix)