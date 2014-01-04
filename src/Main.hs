module Main where

import System.Environment
import System.IO
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Exception
import Control.Monad
import Data.Binary
import GHC.DataSize
import qualified Condor.Index as Index


-- | Commands dispatcher
dispatch :: [(String, [String] -> Index.IndexData -> IO Index.IndexData)]  
dispatch =  [ ("add", addCmd)  
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
    putStrLn $ "Entries count: " ++ show (Index.size idx2)
    writeIndex indexFile idx2   


-- | Read Index data from file
readIndex :: FilePath -> IO Index.IndexData
readIndex p = decodeFile p

-- | Create empty index if can't read from file
readIndexError :: IOError -> IO Index.IndexData
readIndexError _ = return Index.empty

-- | Write index data to the file
writeIndex :: FilePath -> Index.IndexData -> IO ()
writeIndex p i = do encodeFile p i

-- | Command to add all files from given folder to the index 
addCmd :: [String] -> Index.IndexData -> IO Index.IndexData      
addCmd (p:_) idx = do 
    putStrLn $ "Added folder: " ++ p
    ds <- getDirectoryContents p  
    fs <- filterM (fmap not . doesDirectoryExist) ds
    let ps = map ((p++"/")++) fs
    idx2 <- foldM addFile idx ps
    return idx2
addCmd _ _ = error "add command requires path to the documents"

-- | Add file to the index
addFile :: Index.IndexData -> FilePath -> IO Index.IndexData
addFile idx p = do
    putStrLn $ "Load content from: " ++ p
    withFile p ReadMode (\h -> do
        hSetEncoding h utf8_bom  
        contents <- hGetContents h  
        let idx2 = Index.add p contents idx
        size <- recursiveSize idx2
        putStrLn $ "Content length: " ++ show (length contents)
        putStrLn $ "Index size: " ++ show ((size `div` 1000)::Int) ++ "KB"
        return idx2  
        )  
    

-- | Command tosearch index
searchCmd :: [String] -> Index.IndexData -> IO Index.IndexData
searchCmd (t:_) idx = do 
    let result = Index.search idx t
    putStrLn $ "Search term: " ++ show t ++ " found in documents: "
    _ <- mapM putStrLn result
    return idx
searchCmd _ _ = error "search command requires search terms"

            
