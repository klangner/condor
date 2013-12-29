module Condor.Index (
    addDocument
    ) where

type DocName = String
type Text = String
type Index = String

addDocument :: Index -> DocName -> Text -> Index
addDocument idx d c = idx