{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module InvertedIndexLib
    ( createInvertedIndex
    ) where
    
import Data.Aeson (encode, decode, FromJSON, Object, ToJSON)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import GHC.IO.Encoding (setLocaleEncoding, latin1)
import Data.List (nub)
import Data.Map (Map, empty)
import Data.Maybe (isNothing, fromJust)
import Data.Typeable
import GHC.Generics

data WebPageInfoJson = 
  WebPageInfo {
    urlLink :: T.Text,
    textContentWords :: [(T.Text, Int)],
    links :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON WebPageInfoJson
instance FromJSON WebPageInfoJson

data InvertedIndex =
  InvertedIndex { 
    index :: [(T.Text,[T.Text])]
  } deriving (Show, Generic)

instance FromJSON InvertedIndex
instance ToJSON InvertedIndex

isEmptyString :: BB.ByteString -> Bool
isEmptyString x = x == ""

decodeWebPageInfoJson :: BB.ByteString -> Maybe WebPageInfoJson
decodeWebPageInfoJson x = decode x :: Maybe WebPageInfoJson

getLinks :: WebPageInfoJson -> T.Text
getLinks json = urlLink json

getWords :: WebPageInfoJson -> [(T.Text, Int)]
getWords json = textContentWords json

stripOccurence :: [[(a, b)]] -> [[a]]
stripOccurence = map (map fst)

flattenWords :: [[(a, b)]] -> [a]
flattenWords = concatMap (map fst)

urlsForWord :: T.Text -> [(T.Text, [T.Text])] -> [T.Text]
urlsForWord word urlWordsTupleList = 
  let containsWord = filter (\x -> elem word (snd x)) urlWordsTupleList
  in map (\x -> (fst x)) containsWord



createInvertedIndex :: IO ()
createInvertedIndex = do 
  jsonCollectionFile <- BB.readFile "webPageInfo0.txt"
  let jsonList = filter (not . isEmptyString) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = map (decodeWebPageInfoJson) jsonList
  let jsonDecodedList = map (fromJust) (filter (not . isNothing) jsonDecodedListMaybe)
  let links = map (getLinks) jsonDecodedList
  let words = map (getWords) jsonDecodedList
  let wordsClean = (stripOccurence) words
  let urlWordsTuple = zip links wordsClean
  let wordsList = nub ((flattenWords) words)
  let wordsWithUrl = map (\x -> (x,(urlsForWord x urlWordsTuple))) wordsList 
  let invertedIndex = InvertedIndex wordsWithUrl
  let invertedIndexEncoded = encode invertedIndex
  setLocaleEncoding latin1
  BB.appendFile "invertedIndex.txt" invertedIndexEncoded

  