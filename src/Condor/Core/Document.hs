{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Condor.Core.Document
Copyright : Copyright (C) 2014-2016 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Contains basic data structures uses  by other modules.
-}
module Condor.Core.Document
    ( DocName
    , Document(..)
    , mkDocument
    , docName
    , docText
    ) where

import Prelude hiding (concat, map)
import qualified Data.List as List
import Data.Text


-- | Document name
type DocName = Text

-- | Field consists of title and content
data Field = Field Text Text
                    
-- | Document with name and contents 
data Document = Document DocName [Field]


-- | Create simple field from strings
mkField :: Text -> Text -> Field
mkField k v = Field k v

-- | Create simple document with name and single field content.
mkDocument :: Text -> Text -> Document
mkDocument t c = Document t [mkField "content" c]

-- | Get document name
docName :: Document -> Text
docName (Document a _) = a

-- | Get text from all fields
docText :: Document -> Text
docText (Document _ fs) = concat $ List.map (\(Field _ y) -> y) fs