module Main where

import System.Environment
import System.IO
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Exception
import Control.Monad
import Data.Binary
import Condor.Index


-- | Commands dispatcher
dispatch :: [(String, [String] -> Index -> IO Index)]  
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
    idx2 <- action args idx
    -- This line is neccessary since otherwise there is problem with locked file
    -- Lazy evaluation first opens file for saving index 
    putStrLn $ "Entries count: " ++ show (termCount idx2)
    writeIndex indexFile idx2   


-- | Read Index data from file
readIndex :: FilePath -> IO Index
readIndex p = decodeFile p

-- | Create empty index if can't read from file
readIndexError :: IOError -> IO Index
readIndexError _ = return emptyIndex

-- | Write index data to the file
writeIndex :: FilePath -> Index -> IO ()
writeIndex p i = do encodeFile p i

-- | Command to index all files from given folder 
indexCmd :: [String] -> Index -> IO Index      
indexCmd (p:_) idx = do 
    putStrLn $ "Added folder: " ++ p
    ds <- getDirectoryContents p  
    let ps = map ((p++"/")++) ds
    fs <- filterM (fmap not . doesDirectoryExist) ps
    idx2 <- foldM addFile idx fs
    return idx2
indexCmd _ _ = error "add command requires path to the documents"

-- | Add file to the index
addFile :: Index -> FilePath -> IO Index
addFile idx p = do
    putStrLn $ "Load content from: " ++ p
    withFile p ReadMode (\h -> do
        hSetEncoding h utf8
        contents <- hGetContents h  
        putStrLn $ "Content length: " ++ show (length contents)
        let idx2 = addDocument p contents idx
        putStrLn $ "Index entries: " ++ show (termCount idx2)
        return idx2  
        )  
    

-- | Command tosearch index
searchCmd :: [String] -> Index -> IO Index
searchCmd (t:_) idx = do 
    let result = search idx t
    putStrLn $ "Search term: " ++ show t ++ " found in documents: "
    _ <- mapM putStrLn result
    return idx
searchCmd _ _ = error "search command requires search terms"

            
