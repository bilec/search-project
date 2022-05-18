{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module PageRankLib
    ( pageRank
    ) where

import Data.Aeson (encode, decode, FromJSON, Object, ToJSON)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.List (nub, foldl1')
import Data.Map (unionWith, fromList, size, Map, empty, filterWithKey, filter, elems, mapWithKey, map, keys)
import Data.Maybe (isNothing, fromJust)
import GHC.Generics

data WebPageInfoJson = 
  WebPageInfo {
    urlLink :: T.Text,
    textContentWords :: [(T.Text, Int)],
    links :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON WebPageInfoJson
instance FromJSON WebPageInfoJson

data PageRankJson =
  PageRank {
    url :: T.Text,
    pageRankValue :: Double
  } deriving (Show, Generic)

instance ToJSON PageRankJson
instance FromJSON PageRankJson

tuppleToList :: (a, [a]) -> [a]
tuppleToList (x, xx) = (x:xx)

processJson :: WebPageInfoJson -> Map T.Text ([T.Text], Int)
processJson json = 
  let link = urlLink json
      words = textContentWords json
      urls = links json
      forElementInField = Prelude.map (\url -> (url, ([link], 0))) urls
  in fromList ((link, ([], length words)):forElementInField)

combinePair :: ([T.Text], Int) -> ([T.Text], Int) -> ([T.Text], Int)
combinePair a b = (fst a ++ fst b, snd a + snd b)

myUnionWith :: Map T.Text ([T.Text], Int) -> Map T.Text ([T.Text], Int) -> Map T.Text ([T.Text], Int)
myUnionWith a b = unionWith (combinePair) a b

numberOfAllPages :: [(T.Text, ([T.Text], Int))] -> Int
numberOfAllPages xs = (size . fromList) xs

numberOfAllPages' :: Map T.Text ([T.Text], Int) -> Int
numberOfAllPages' xs = size xs

decodeWebPageInfoJson :: BB.ByteString -> Maybe WebPageInfoJson
decodeWebPageInfoJson x = decode x :: Maybe WebPageInfoJson

first :: ([T.Text], Int, Double) -> [T.Text]
first (a, _, _) = a

second :: ([T.Text], Int, Double) -> Int
second (_, b, _) = b

third :: ([T.Text], Int, Double) -> Double
third (_, _, c) = c

calculateSinglePageRank ::  (T.Text, [T.Text]) -> Map T.Text ([T.Text], Int, Double) -> Int -> Double
calculateSinglePageRank page pageRanks numberOfPages =
  let dampingFactor = 0.85
      firstPart = (1 - dampingFactor) / (fromIntegral numberOfPages)
      pageUrl = fst page
      pageIncoming = snd page
      incomingPages = filterWithKey (\k _ -> elem k pageIncoming) pageRanks
      incomingPagesElems = elems incomingPages
      secondPartList = Prelude.map (\x -> (third x) / (fromIntegral (second x))) incomingPagesElems
      secondPart = (foldl1' (+) secondPartList) * dampingFactor

  in firstPart + secondPart

calculatePageRanks :: Map T.Text ([T.Text], Int) -> Int -> Int -> Map T.Text ([T.Text], Int, Double)
calculatePageRanks pageStats numberOfPages numberOfIterations =
  let oldPageRanks = Data.Map.map (\x -> ((fst x), (snd x), (1.0 / (fromIntegral numberOfPages)))) pageStats
  in calculatePageRanksIteration numberOfIterations numberOfPages oldPageRanks

calculatePageRanksIteration :: Int -> Int -> Map T.Text ([T.Text], Int, Double) -> Map T.Text ([T.Text], Int, Double)
calculatePageRanksIteration 0 _ oldPageRanks = oldPageRanks
calculatePageRanksIteration iterations numberOfPages oldPageRanks = 
  calculatePageRanksIteration (iterations - 1) numberOfPages (mapWithKey (\k v -> ((first v), (second v), (calculateSinglePageRank (k, (first v)) oldPageRanks numberOfPages))) oldPageRanks)

solveSinks :: Map T.Text ([T.Text], Int) -> Int -> Map T.Text ([T.Text], Int)
solveSinks pageStats numberOfPages =
  let sinks = Data.Map.filter (\v -> (snd v) == 0) pageStats
      sinksUpdated = Data.Map.map (\v -> ((fst v), numberOfPages)) sinks
      pageStatsSinkOutgoing = unionWith (combinePair) sinksUpdated pageStats
      sinkKeys = keys sinksUpdated
  in Data.Map.map (\v -> ( (nub(((fst v) ++ sinkKeys))) , (snd v) ) ) pageStatsSinkOutgoing

pageRankFileName :: String
pageRankFileName = "pageRank.txt"

appendPageRankJsonsToFile :: [BB.ByteString] -> IO ()
appendPageRankJsonsToFile [] = return ()
appendPageRankJsonsToFile (x:xs) = do
  BB.appendFile pageRankFileName "\n"
  BB.appendFile pageRankFileName x
  appendPageRankJsonsToFile xs

pageRank :: IO ()
pageRank = do
  jsonCollectionFile <- BB.readFile "webPageInfo.txt"
  let jsonList = Prelude.filter (not . BB.null) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = Prelude.map (decodeWebPageInfoJson) jsonList
  let jsonDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) jsonDecodedListMaybe)
  let processedJsonList = Prelude.map (processJson) jsonDecodedList
  let pagesStats = foldl (myUnionWith) empty processedJsonList
  let numberOfPages = numberOfAllPages' pagesStats
  let pageStatsSinkSolved = solveSinks pagesStats numberOfPages

  let pageRanks = calculatePageRanks pageStatsSinkSolved numberOfPages 10
  let pageRankJsons = elems $ mapWithKey (\k v -> PageRank k (third v)) pageRanks
  let encodedPageRankJsons = Prelude.map (Data.Aeson.encode) pageRankJsons
  let encodedPageRankLinesJsons = Prelude.map (\x -> BB.cons '\n' x) encodedPageRankJsons
  let toWrite = foldr1 (<>) encodedPageRankLinesJsons

  BB.writeFile pageRankFileName toWrite

  print "Finished calculating pageRank!"  