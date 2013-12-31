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
import Condor.Text


type DocName = String
type Text = String
data Index = Index { index :: Map.Map String [String]
                   }
                   deriving (Show)


-- | Create empty index
empty :: Index
empty = Index Map.empty


-- | Add document to the index
add :: DocName -> Text -> Index -> Index
add d c ix = Index $ foldl f (index ix) (tokenize c)
    where f i t = case Map.lookup t i of 
                    Just a -> Map.insert t (d:a) i
                    Nothing -> Map.insert t [d] i


-- | search term in the index
search :: Index -> Text -> [DocName]
search ix s = case Map.lookup (strToLower s) (index ix) of
                Just a -> a
                Nothing -> []
                 

-- | Get index size
size :: Index -> Int
size ix = Map.size (index ix)