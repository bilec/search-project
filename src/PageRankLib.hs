{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module PageRankLib
    ( pageRank
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map, singleton, unionWith, map, filter, keysSet, insert, union, toList, delete, (!), size, mapWithKey, elems)
import Data.Set (Set, singleton, union, empty, insert, size, toList)
import Data.List (foldr1)

import GHC.Generics (Generic)

import Data.Map (elemAt)

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

first :: ((Set T.Text), Int, Double) -> (Set T.Text)
first (a, _, _) = a

second :: ((Set T.Text), Int, Double) -> Int
second (_, b, _) = b

third :: ((Set T.Text), Int, Double) -> Double
third (_, _, c) = c

decodeWebPageInfoJson :: BB.ByteString -> Maybe WebPageInfoJson
decodeWebPageInfoJson x = decode x :: Maybe WebPageInfoJson

processWebPageInfoJson :: WebPageInfoJson -> Map T.Text ((Set T.Text), Int)
processWebPageInfoJson json =
  let link = urlLink json
      outcomingLinks = links json
      incomingLinks = Prelude.map (\x -> Data.Map.singleton (x) ((Data.Set.singleton link), 0)) outcomingLinks
  in if (Prelude.null incomingLinks)
    then
      Data.Map.singleton link ((Data.Set.empty), Prelude.length outcomingLinks)
    else 
      let parentLink = Data.Map.singleton link ((Data.Set.empty), Prelude.length outcomingLinks)
          childrenLinks = Data.List.foldr1 (Data.Map.unionWith (myOwnUnionWith)) incomingLinks
      in Data.Map.unionWith (myOwnUnionWith) childrenLinks parentLink

myOwnUnionWith :: ((Set T.Text), Int) -> ((Set T.Text), Int) -> ((Set T.Text), Int)
myOwnUnionWith (aSet, aNumber) (bSet, bNumber) = ((Data.Set.union aSet bSet), (aNumber + bNumber))

hypotheticalPage :: T.Text
hypotheticalPage = "hypotheticalPage"

solveDanglingLinks :: Map T.Text ((Set T.Text), Int) -> Map T.Text ((Set T.Text), Int)
solveDanglingLinks graph =
  let danglingLinks = Data.Map.filter (\x -> (snd x) == 0) graph
      notDanglingLinks = Data.Map.map (\x -> ((fst x), 1)) danglingLinks
      notDanglingLinksWithHypotheticalPage = Data.Map.insert hypotheticalPage ((Data.Set.insert hypotheticalPage (keysSet notDanglingLinks)), 1) notDanglingLinks
  in Data.Map.union notDanglingLinksWithHypotheticalPage graph

calculatePageRankForAllPages :: Map T.Text ((Set T.Text), Int) -> Map T.Text Double
calculatePageRankForAllPages graph =
  let numberOfAllPages = Data.Set.size (Data.Map.keysSet graph)
      numberOfAllPagesDouble = Prelude.fromIntegral numberOfAllPages
      zeroPR = (1.0 / (numberOfAllPagesDouble))
      graphWithPR = Data.Map.map (\x -> ((fst x), (snd x), zeroPR)) graph

      calculatedPageRank = calculatePageRankForAllPagesTillIteriation 50 dampingFactorNumber graphWithPR
  in removeHypotheticalPage (Data.Map.map (third) calculatedPageRank)

calculatePageRankForAllPagesTillIteriation :: Int -> Double -> Map T.Text ((Set T.Text), Int, Double) -> Map T.Text ((Set T.Text), Int, Double)
calculatePageRankForAllPagesTillIteriation 0 _ graph = graph
calculatePageRankForAllPagesTillIteriation iteration dampingFactor graph =
  let newGraph = Data.Map.mapWithKey (\key value -> ((first value), (second value), (calculatePageRankForOnePage key value dampingFactor graph))) graph
      error = errorBetweenOldAndNewPageRank graph newGraph
  in if (error > 0.00001)
    then
      calculatePageRankForAllPagesTillIteriation (iteration - 1) dampingFactor newGraph
    else newGraph

calculatePageRankForOnePage :: T.Text -> ((Set T.Text), Int, Double) -> Double -> Map T.Text ((Set T.Text), Int, Double) -> Double
calculatePageRankForOnePage key value dampingFactor graph =
  let incomingLinks = first value
      outcomingLinksNumber = second value
      numberOfAllPagesDouble = Prelude.fromIntegral $ Data.Map.size graph
      incomingLinksPRAndOutcomingLinksNumber = Prelude.map (\x -> pageRankAndOutcomingLinksNumberFromMap x graph) (Data.Set.toList incomingLinks)
      prDividedByOutcomingLinksNumber = Prelude.map (divideTupple) incomingLinksPRAndOutcomingLinksNumber
      sumOfPRDividedByOutcomingLinksNumber = Prelude.sum prDividedByOutcomingLinksNumber
  in ((1.0 - dampingFactor) ) + (sumOfPRDividedByOutcomingLinksNumber * dampingFactor)

pageRankAndOutcomingLinksNumberFromMap :: T.Text -> Map T.Text ((Set T.Text), Int, Double) -> (Double, Double)
pageRankAndOutcomingLinksNumberFromMap key graph = 
  let value = graph ! key
  in ((Prelude.fromIntegral (second value)), (third value))

divideTupple :: (Double, Double) -> Double
divideTupple (a,b) = b / a

removeHypotheticalPage :: Map T.Text Double -> Map T.Text Double
removeHypotheticalPage graph = Data.Map.delete hypotheticalPage graph

errorBetweenOldAndNewPageRank :: Map T.Text ((Set T.Text), Int, Double) -> Map T.Text ((Set T.Text), Int, Double) -> Double
errorBetweenOldAndNewPageRank oldGraph newGraph = 
  let oldGraphPR = removeHypotheticalPage (Data.Map.map (third) oldGraph)
      newGraphPR = removeHypotheticalPage (Data.Map.map (third) newGraph)
      combinedGraph = Data.Map.unionWith (\x y -> (Prelude.abs (x - y))) oldGraphPR newGraphPR
  in Prelude.sum (Data.Map.elems combinedGraph)

dampingFactorNumber :: Double
dampingFactorNumber = 0.85

pageRankFileName :: String
pageRankFileName = "pageRank.txt"

pageRank :: IO ()
pageRank = do
  jsonCollectionFile <- BB.readFile "webPageInfo.txt"
  let jsonList = Prelude.filter (not . BB.null) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = Prelude.map (decodeWebPageInfoJson) jsonList
  let jsonDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) jsonDecodedListMaybe)

  let processedJsonList = Prelude.map (processWebPageInfoJson) jsonDecodedList
  let graphWithDanglingNodes = Data.List.foldr1 (Data.Map.unionWith (myOwnUnionWith)) processedJsonList 
  let graph = solveDanglingLinks graphWithDanglingNodes

  let pageRankForAllPages = calculatePageRankForAllPages graph
  let pageRankJsonList = Prelude.map (\(x, y) -> PageRank x y) (Data.Map.toList pageRankForAllPages)
  let encodedPageRankJsonList = Prelude.map (encode) pageRankJsonList
  let encodedPageRankToPrint = BB.unlines encodedPageRankJsonList

  BB.writeFile pageRankFileName encodedPageRankToPrint

  print "Finished calculating pageRank!"