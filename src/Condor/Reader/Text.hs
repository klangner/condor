{-# LANGUAGE BangPatterns #-}
{- |
Module : Condor.Reader.Text
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Reader for text files. Strict version.
Uses bang patters to force hGetContents to read the whole file.
-}
module Condor.Reader.Text(readDocument) where

import System.IO
import Condor.Commons.Document (Document, docFromStrings)

    
-- | read text as UTF8 and return as document
readDocument :: FilePath -> IO Document
readDocument path = do
    withFile path ReadMode (\h -> do
        hSetEncoding h utf8
        !contents <- hGetContents h  
        hClose h
        return $ docFromStrings path contents  
        )      
    