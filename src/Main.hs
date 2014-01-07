module Main where

import System.Environment
import System.Directory (removeFile, renameFile)
import Control.Exception
import Control.Monad
import Data.Binary
import Condor.Index.Memory
import qualified Condor.Readers.Text as TextReader
import IO


-- | Commands dispatcher
dispatch :: [(String, [String] -> Index -> IO ())]  
dispatch =  [ ("index", indexCmd)  
            , ("search", searchCmd)  
            ]
indexFile :: FilePath            
indexFile = "index.db"

-- This is dummy main function (not working yet)
main :: IO ()
main = do
    idx <- (readIndex indexFile) `catch` readIndexError
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args idx


-- | Read Index data from file
readIndex :: FilePath -> IO Index
readIndex p = {-# SCC "read" #-} decodeFile p

-- | Create empty index if can't read from file
readIndexError :: IOError -> IO Index
readIndexError _ = return emptyIndex

-- | Silent IO error
nullError :: IOError -> IO ()
nullError _ = return ()

-- | Write index data to the file
writeIndex :: FilePath -> Index -> IO ()
writeIndex p idx = do
    putStrLn $ "Entries count: " ++ show (termCount idx)
    let temp = p ++ ".temp" 
    encodeFile temp idx
    removeFile p `catch` nullError
    renameFile temp p

-- | Command to index all files from given folder 
indexCmd :: [String] -> Index -> IO ()      
indexCmd [p] idx = do 
    idx2 <- indexFolder False idx p
    writeIndex indexFile idx2   
indexCmd [p, "-r"] idx = do 
    idx2 <- indexFolder True idx p
    writeIndex indexFile idx2
indexCmd _ _ = error "add command requires path to the documents"

-- | Index given folder 
indexFolder :: Bool -> Index -> FilePath -> IO Index      
indexFolder False idx p = do 
    putStrLn $ "Added folder: " ++ p
    fs <- listFiles p
    idx2 <- foldM addFile idx fs
    return idx2
indexFolder True idx p = do 
    idx2 <- indexFolder False idx p
    ds <- listDirs p
    idx3 <- foldM (indexFolder True) idx2 ds
    return idx3

-- | Add file to the index
addFile :: Index -> FilePath -> IO Index
addFile idx p = do
    doc <- TextReader.readDocument p
    let idx2 = addDocument doc idx
    return idx2  
     

-- | Command tosearch index
searchCmd :: [String] -> Index -> IO ()
searchCmd (t:_) idx = do 
    let result = {-# SCC "search" #-} search idx t
    putStrLn $ "Search term: " ++ show t ++ " found in documents: "
    _ <- mapM (putStrLn . show) result
    return ()
searchCmd _ _ = error "search command requires search terms"

            
