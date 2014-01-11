{- |
Module : Condor.Commons.Document
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Contains basic data structures uses  by other modules.
-}
module Condor.Commons.Document 
    ( DocName
    , Document(..)
    , docFromStrings
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
fieldFromStrings :: String -> String -> Field
fieldFromStrings k v = Field (pack k) (pack v) 

-- | Create simple document with name and single field content.
docFromStrings :: String -> String -> Document
docFromStrings t c = Document (pack t) [fieldFromStrings "content" c]

-- | Get document name
docName :: Document -> Text
docName (Document a _) = a

-- | Get text from all fields
docText :: Document -> Text
docText (Document _ fs) = concat $ List.map (\(Field _ y) -> y) fs