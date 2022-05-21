{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module SearchLib
    (search
    ) where
        
import Data.Aeson (encode, decode, FromJSON, Object, ToJSON)

import qualified Data.ByteString.Lazy.Char8 as BB
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Map (Map, map, member)
import Data.Set (Set, map)
import Data.Maybe (fromJust)
import Data.Text (pack, toLower)
import GHC.Generics

newtype InvertedIndex =
    InvertedIndex {
        index :: Map T.Text (Set T.Text)
    } deriving (Show, Generic)

instance FromJSON InvertedIndex
instance ToJSON InvertedIndex

decodeInvertedIndex :: BB.ByteString -> Maybe InvertedIndex
decodeInvertedIndex x = decode x :: Maybe InvertedIndex

findWord :: T.Text -> Map T.Text (Set T.Text) -> Set T.Text
findWord word linksList = linksList M.! word

search :: IO ()
search = do
    indexFile <- BB.readFile "invertedIndex.txt"
    let invIndexJson = decodeInvertedIndex indexFile
    
    print "Please insert text to search:"
    userInput <- getLine
    let invIndex = index (fromJust invIndexJson)
    if member (pack userInput) invIndex 
        then print $ findWord (pack userInput) invIndex
        else print "No result"