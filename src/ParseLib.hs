{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module ParseLib
    ( parse
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.List.Split (splitOn)
import Data.List.Unique (uniq)
import Data.List (findIndex, sort, group)
import Data.Foldable (for_)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Maybe (isNothing, fromJust)
import Data.Typeable (typeOf)

import GHC.IO.Encoding (setLocaleEncoding, latin1)
import GHC.Generics

import Text.HTML.TagSoup (parseTags, innerText, sections, Tag, isTagOpenName, isTagCloseName, isTagText, fromAttrib)


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
    textContentWords :: [T.Text],
    links :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON WebPageInfoJson
instance ToJSON WebPageInfoJson

checkBody :: [Tag T.Text] -> Bool
checkBody tags = 
  let openTagPosition = findIndex (isTagOpenName "body") tags
      closeTagPosition = findIndex (isTagCloseName "body") tags
  in 
    if isNothing openTagPosition || isNothing closeTagPosition
    then False
    else openTagPosition < closeTagPosition

onlyBody :: [Tag T.Text] -> [Tag T.Text]
onlyBody tags = reverse (dropWhile (not . isTagCloseName "body") (reverse (dropWhile (not . isTagOpenName "body") tags)))

isTagOpenAndTagTexAndTagClose :: T.Text -> Tag T.Text -> Tag T.Text -> Tag T.Text -> Bool
isTagOpenAndTagTexAndTagClose name tagA tagB tagC = isTagOpenName name tagA && isTagText tagB && isTagCloseName name tagC

dropScriptAndStyle :: [Tag T.Text] -> [Tag T.Text]
dropScriptAndStyle [] = []
dropScriptAndStyle (a:[]) = (a:[])
dropScriptAndStyle (a:b:[]) = (a:b:[])
dropScriptAndStyle (a:b:c:[]) = if (isTagOpenAndTagTexAndTagClose "script" a b c) || (isTagOpenAndTagTexAndTagClose "style" a b c)
  then []
  else (a:b:c:[])
dropScriptAndStyle (a:b:c:xs) = if (isTagOpenAndTagTexAndTagClose "script" a b c) || (isTagOpenAndTagTexAndTagClose "style" a b c)
  then dropScriptAndStyle xs
  else a : dropScriptAndStyle (b:c:xs)

isCorrectUrl :: T.Text -> Bool
isCorrectUrl url = T.isPrefixOf "http://" url || T.isPrefixOf "https://" url

extractLinks :: [Tag T.Text] -> [T.Text]
extractLinks tags = uniq (filter (\x -> x /= "" && isCorrectUrl x) (map (fromAttrib "href") (filter (isTagOpenName "a") tags)))

processJson :: String -> IO ()
processJson json = 
  do
    let jsonPacked = BB.pack json
    let webPageJsonMaybe = decode jsonPacked :: Maybe WebPageJson

    if (isNothing webPageJsonMaybe)
    then return ()
    else 
      do
        let webPageParsed = fromJust webPageJsonMaybe
        let urlLink = url webPageParsed

        let htmlContentText = html_content webPageParsed
        let tags = parseTags htmlContentText

        if (not $ checkBody tags)
          then return ()
          else 
            do
              let body = onlyBody tags
              let bodyWithoutScriptAndStyle = dropScriptAndStyle body

              let bodyWords = T.words $ T.strip $ innerText bodyWithoutScriptAndStyle
              let urlLinks = extractLinks bodyWithoutScriptAndStyle

              let webPageInfo = WebPageInfo urlLink bodyWords urlLinks
              let webPageInfoEncoded = BB.unpack $ encode webPageInfo

              setLocaleEncoding latin1
              appendFile "webPageInfo.txt" webPageInfoEncoded

parse :: IO ()
parse = do 
  jsonCollectionFile <- readFile "C:\\Users\\mbilka\\Desktop\\collection.jl"
  let jsonList = splitOn "\n" jsonCollectionFile
  for_ jsonList processJson

  print "Finished parsing json/html!"