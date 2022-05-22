{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module SearchLib
    (search
    ) where
        
import Data.Aeson (encode, decode, FromJSON, Object, ToJSON)

import qualified Data.ByteString.Lazy.Char8 as BB
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Map (Map, map, member)
import Data.Set (Set, map, member)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (pack, toLower)
import Data.Maybe (isNothing, fromJust)
import GHC.Generics (Generic)

newtype InvertedIndex =
    InvertedIndex {
        index :: Map T.Text (Set T.Text)
    } deriving (Show, Generic)

instance FromJSON InvertedIndex
instance ToJSON InvertedIndex

data PageRankJson =
  PageRank {
    url :: T.Text,
    pageRankValue :: Double
  } deriving (Show, Generic)

instance ToJSON PageRankJson
instance FromJSON PageRankJson

decodeInvertedIndex :: BB.ByteString -> Maybe InvertedIndex
decodeInvertedIndex x = decode x :: Maybe InvertedIndex

decodePageRankJson :: BB.ByteString -> Maybe PageRankJson
decodePageRankJson x = decode x :: Maybe PageRankJson

findWord :: T.Text -> Map T.Text (Set T.Text) -> Set T.Text
findWord word linksList = linksList M.! word

sortLinks :: Set T.Text -> M.Map T.Text Double -> [T.Text]
sortLinks links pageRank = Prelude.map (fst) (Data.List.sortOn (fst) (M.toList (M.filterWithKey (\key _ -> (Data.Set.member key links)) pageRank)))

search :: IO ()
search = do
    indexFile <- BB.readFile "invertedIndex.txt"
    pageRankFile <- BB.readFile "pageRank.txt"
    let invIndexJson = decodeInvertedIndex indexFile
    
    let pageRankList = Prelude.filter (not . BB.null) (BB.lines pageRankFile)
    let pageRankDecodedListMaybe = Prelude.map (decodePageRankJson) pageRankList
    let pageRankDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) pageRankDecodedListMaybe)
    let pageRankDecodedAndParsedList = Prelude.map (\x -> ((url x), (pageRankValue x))) pageRankDecodedList
    let pageRankMap = M.fromList pageRankDecodedAndParsedList
    
    print "Please insert text to search:"
    userInput <- getLine
    let invIndex = index (fromJust invIndexJson)
    if M.member (pack userInput) invIndex 
        then print $ sortLinks (findWord (pack userInput) invIndex) pageRankMap
        else print "No result"