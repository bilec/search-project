{-# LANGUAGE OverloadedStrings #-}

module ParseLib ( 
  parse
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import Data.List (findIndex, sort, group, nub)
import Data.Maybe (isNothing, fromJust, Maybe(Nothing))
import Text.HTML.TagSoup (parseTags, innerText, Tag, isTagOpenName, isTagCloseName, isTagText, fromAttrib)

import TypeClassesLib (WebPageJson(..), WebPageInfoJson(..), decodeWebPageJson, encodeWebPageInfoJson)

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

isCorrectUrlHttpOrHttps :: T.Text -> T.Text -> Int -> Bool
isCorrectUrlHttpOrHttps url protocol number = 
  let prefix = T.isPrefixOf protocol url 
      urlBody = T.drop number url
      containsDot = T.isInfixOf "." urlBody
      notOnlyDot = (not . T.null) (T.filter (/= '.') urlBody)
  in prefix && containsDot && notOnlyDot

isCorrectUrlHttp :: T.Text -> Bool
isCorrectUrlHttp url = isCorrectUrlHttpOrHttps url "http://" 7

isCorrectUrlHttps :: T.Text -> Bool
isCorrectUrlHttps url = isCorrectUrlHttpOrHttps url "https://" 8

isCorrectUrl :: T.Text -> Bool
isCorrectUrl url = (isCorrectUrlHttp url) || (isCorrectUrlHttps url)

extractLinks :: [Tag T.Text] -> [T.Text]
extractLinks tags = nub (filter (\x -> x /= "" && isCorrectUrl x) (map (fromAttrib "href") (filter (isTagOpenName "a") tags)))

processJson :: WebPageJson -> Maybe WebPageInfoJson
processJson webPageJson =
  let urlLink = url webPageJson
      content = html_content webPageJson
      contentTags = parseTags content
  in 
    if (not $ checkBody contentTags)
      then Nothing
      else 
        let body = onlyBody contentTags
            bodyWithoutScriptAndStyle = dropScriptAndStyle body

            bodyWords = group $ sort $ T.words $ T.strip $ innerText bodyWithoutScriptAndStyle
            bodyWordsWithOccurrence = map (\x -> (head x, length x)) bodyWords
            urlLinks = extractLinks bodyWithoutScriptAndStyle
        in Just $ WebPageInfo urlLink bodyWordsWithOccurrence urlLinks

parse :: IO ()
parse = do 
  jsonCollectionFile <- BB.readFile "collection.jl"
  let jsonList = Prelude.filter (not . BB.null) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = Prelude.map (decodeWebPageJson) jsonList
  let jsonDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) jsonDecodedListMaybe)
  let processedJsonListMaybe = Prelude.map (processJson) jsonDecodedList
  let processedJsonList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) processedJsonListMaybe)
  let encodedJsonList = Prelude.map (encodeWebPageInfoJson) processedJsonList
  let toWrite = BB.unlines encodedJsonList
  
  BB.writeFile "webPageInfo.txt" toWrite

  print "Finished parsing json/html!"