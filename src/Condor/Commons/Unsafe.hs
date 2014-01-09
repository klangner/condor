{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module : Condor.Commons.Unsafe
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains code which is necessary but should be removed if possible.
The warning here are turn off.
-}
module Condor.Commons.Unsafe
    ( 
    ) where

import Data.Text
import Data.Binary
import Data.Text.Encoding (encodeUtf8, decodeUtf8)


-- | This instance is necessary so Index can be serialized to and from disk using binary format
instance Binary Text where
     put i = do put (encodeUtf8 i)
     get = do i <- get
              return $ decodeUtf8 i                        

