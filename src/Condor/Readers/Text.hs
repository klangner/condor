{-# LANGUAGE BangPatterns #-}
{- |
Module : Condor.Readers.Text
Copyright : Copyright (C) 2014 Krzysztof Langner
License : The MIT License (MIT)

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Reader for text files. Strict version.
Uses bang patters to force hGetContents to read file
-}
module Condor.Readers.Text(readDocument) where

import System.IO
import Condor.Commons.DataTypes (Document, docFromStrings)

    
-- | read text as UTF8 and return as document
readDocument :: FilePath -> IO Document
readDocument path = do
    withFile path ReadMode (\h -> do
        hSetEncoding h utf8
        !contents <- hGetContents h  
        hClose h
        return $ docFromStrings path contents  
        )      
    