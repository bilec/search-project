{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings #-}

module ParseLib
    ( parse
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.List.Split (splitOn)
import Data.List (findIndex, sort, group, nub)
import Data.Foldable (for_)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Maybe (isNothing, fromJust, Maybe(Nothing))
import Data.Typeable (typeOf)
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
    textContentWords :: [(T.Text, Int)],
    links :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON WebPageInfoJson
instance ToJSON WebPageInfoJson

isEmptyString :: BB.ByteString -> Bool
isEmptyString x = (x == "")

decodeWebPageJson :: BB.ByteString -> Maybe WebPageJson
decodeWebPageJson x = decode x :: Maybe WebPageJson

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
extractLinks tags = nub (filter (\x -> x /= "" && isCorrectUrl x) (map (fromAttrib "href") (filter (isTagOpenName "a") tags)))

processJson :: WebPageJson -> Maybe WebPageInfoJson
processJson webPageJson = do
  let urlLink = url webPageJson
  let content = html_content webPageJson
  let contentTags = parseTags content
  if (not $ checkBody contentTags)
    then Nothing
    else 
      do
        let body = onlyBody contentTags
        let bodyWithoutScriptAndStyle = dropScriptAndStyle body

        let bodyWords = group $ sort $ T.words $ T.strip $ innerText bodyWithoutScriptAndStyle
        let bodyWordsWithOccurrence = map (\x -> (head x, length x)) bodyWords
        let urlLinks = extractLinks bodyWithoutScriptAndStyle
        Just $ WebPageInfo urlLink bodyWordsWithOccurrence urlLinks

parse :: IO ()
parse = do 
  jsonCollectionFile <- BB.readFile "collection.jl"
  let jsonList = Prelude.filter (not . isEmptyString) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = Prelude.map (decodeWebPageJson) jsonList
  let jsonDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) jsonDecodedListMaybe)
  let processedJsonListMaybe = Prelude.map (processJson) jsonDecodedList
  let processedJsonList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) processedJsonListMaybe)
  let encodedJsonList = Prelude.map (Data.Aeson.encode) processedJsonList
  let encodedJsonLinesList = Prelude.map (\x -> BB.cons '\n' x) encodedJsonList
  let toWrite = foldr1 (<>) encodedJsonLinesList
  
  writeFile "webPageInfo.txt" ""
  BB.writeFile "webPageInfo.txt" toWrite

  print "Finished parsing json/html!"