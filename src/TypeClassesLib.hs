{-# LANGUAGE DeriveGeneric #-}

module TypeClassesLib ( 
    WebPageJson (..),
    WebPageInfoJson (..),
    InvertedIndex (..),
    PageRankJson (..),
    encodeWebPageJson,
    decodeWebPageJson,
    encodeWebPageInfoJson,
    decodeWebPageInfoJson,
    encodeInvertedIndex,
    decodeInvertedIndex,
    encodePageRankJson,
    decodePageRankJson,
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)

data WebPageJson =
    WebPageJson { 
        url :: T.Text,
        html_content :: T.Text  
    } deriving (Show, Generic)

instance FromJSON WebPageJson
instance ToJSON WebPageJson

data WebPageInfoJson = 
    WebPageInfo {
        urlLink :: T.Text,
        textContentWords :: [(T.Text, Int)],
        links :: [T.Text]
    } deriving (Show, Generic)

instance FromJSON WebPageInfoJson
instance ToJSON WebPageInfoJson

newtype InvertedIndex =
    InvertedIndex {
        index :: Map T.Text (Set T.Text)
    } deriving (Show, Generic)

instance FromJSON InvertedIndex
instance ToJSON InvertedIndex

data PageRankJson =
    PageRank {
        prUrl :: T.Text,
        pageRankValue :: Double
    } deriving (Show, Generic)

instance ToJSON PageRankJson
instance FromJSON PageRankJson


encodeWebPageJson :: WebPageJson -> BB.ByteString
encodeWebPageJson = encode

decodeWebPageJson :: BB.ByteString -> Maybe WebPageJson
decodeWebPageJson x = decode x :: Maybe WebPageJson


encodeWebPageInfoJson :: WebPageInfoJson -> BB.ByteString
encodeWebPageInfoJson = encode

decodeWebPageInfoJson :: BB.ByteString -> Maybe WebPageInfoJson
decodeWebPageInfoJson x = decode x :: Maybe WebPageInfoJson


encodeInvertedIndex :: InvertedIndex -> BB.ByteString
encodeInvertedIndex = encode

decodeInvertedIndex :: BB.ByteString -> Maybe InvertedIndex
decodeInvertedIndex x = decode x :: Maybe InvertedIndex


encodePageRankJson :: PageRankJson -> BB.ByteString
encodePageRankJson = encode

decodePageRankJson :: BB.ByteString -> Maybe PageRankJson
decodePageRankJson x = decode x :: Maybe PageRankJson
