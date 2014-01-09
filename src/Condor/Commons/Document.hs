{- |
Module : Condor.Commons.Document
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Common to all modules data types definitions.
-}
module Condor.Commons.Document 
    ( DocName
    , Document(..)
    , docFromStrings
    , docName
    , docText
    ) where

import qualified Data.Text as T


type DocName = T.Text

-- | Field consists of title and content
data Field = Field T.Text T.Text
                    
-- | Document with name and contents 
data Document = Document DocName [Field]


-- | Create simple field from strings
fieldFromStrings :: String -> String -> Field
fieldFromStrings k v = Field (T.pack k) (T.pack v) 

-- | Create simple document with name and single field content.
docFromStrings :: String -> String -> Document
docFromStrings t c = Document (T.pack t) [fieldFromStrings "content" c]

-- | Get document name
docName :: Document -> T.Text
docName (Document a _) = a

-- | Get text from all fields
docText :: Document -> T.Text
docText (Document _ fs) = T.concat $ map (\(Field _ y) -> y) fs