module Main where

import System.IO
import System.Environment


-- This is dummy main function (not working yet)
main :: IO ()
main = do    
    args <- getArgs  
    putStrLn "The arguments are:"  
    _ <- mapM putStrLn args   
    withFile "samples/haskell.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)  
