{- |
Module : Condor.Definition
Copyright : Copyright (C) 2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Common to all modules data types definitions.
-}
module Condor.Definition 
    ( DocName
    , Document(..)
    , docFromStrings
    ) where

import qualified Data.Text as T

type DocName = T.Text
                    
-- | Document with name and contents 
data Document = Document DocName T.Text


-- | Create simple document with title and content
docFromStrings :: String -> String -> Document
docFromStrings t c = Document (T.pack t) (T.pack c) 