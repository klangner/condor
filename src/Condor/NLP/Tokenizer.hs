{- |
Module : Condor.NLP.Tokenizer
Copyright : Copyright (C) 2013-2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

This module contains functions which parses text into tokens. 
tokens are not normalized. If you need all tokens from the document then 
check function "tokenize". If you need only words (na dots, numbers etc.) 
then check function "getWords".

-}
module Condor.NLP.Tokenizer 
    ( Token(..)
    , foldCase
    , getWords
    , tokenize
    ) where
    
import qualified Data.Text as T
import Data.Char

-- | Token type
data Token = Word T.Text
           | Number T.Text
           | Punctuation Char
           | Symbol Char
           | Whitespace
           | Unknown Char
           deriving (Eq, Show)


-- | Split text into tokens
--
-- > tokenize "one two." == [Word "one", Whitespace, Word "two", "Separator "."] 
tokenize :: T.Text -> [Token]
tokenize xs = case allParser xs of
                [(v, out)] -> (v:tokenize out)
                _ -> []
                
-- | Exctract all words from tokens
--
-- > getWords "one two." == ["one", "two"] 
getWords :: [Token] -> [T.Text]
getWords [] = []
getWords (x:xs) = case x of
                        Word a -> (a: getWords xs)
                        _ -> getWords xs
                        
-- | Convert all words to the same case
foldCase :: [T.Text] -> [T.Text]
foldCase = map T.toCaseFold                                          
    

-- | Parser type
type Parser = T.Text -> [(Token, T.Text)]

-- | Parse word
wordParser :: Parser
wordParser xs | T.null xs = []
              | isLetter (T.head xs) = [(Word (T.takeWhile isAlphaNum xs), T.dropWhile isAlphaNum xs)]
              | otherwise = []

-- | Parse number
numberParser :: Parser
numberParser xs | T.null xs = []
                | isDigit (T.head xs) = [(Number (T.takeWhile isDigit xs), T.dropWhile isDigit xs)]
                | otherwise = []

-- | Parse punctuation
punctuationParser :: Parser
punctuationParser xs | T.null xs = []
                     | isPunctuation (T.head xs) = [(Punctuation (T.head xs), (T.tail xs))]
                     | otherwise = []

-- | Parse symbol
symbolParser :: Parser
symbolParser xs | T.null xs = []
                | isSymbol (T.head xs) = [(Symbol (T.head xs), (T.tail xs))]
                | otherwise = []
                            
-- | Parse whitespaces
spaceParser :: Parser
spaceParser xs | T.null xs = []
               | isSpace (T.head xs) = [(Whitespace, T.dropWhile isSpace xs)]
               | otherwise = []
                            
-- | Parse single char
charParser :: Parser
charParser xs | T.null xs = []
              | otherwise = [(Unknown (T.head xs), T.tail xs)]

-- | Apply all parsers
allParser :: Parser
allParser xs = case wordParser xs of
                [(v, out)] -> [(v, out)]
                _ -> case numberParser xs of               
                        [(v, out)] -> [(v, out)]
                        _ -> case punctuationParser xs of               
                            [(v, out)] -> [(v, out)]
                            _ -> case symbolParser xs of               
                                [(v, out)] -> [(v, out)]
                                _ -> case spaceParser xs of               
                                    [(v, out)] -> [(v, out)]
                                    _ -> charParser xs
                