{-# LANGUAGE OverloadedStrings #-}

module InvertedIndexLib (
  createInvertedIndex
) where
    
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB

import GHC.IO.Encoding (setLocaleEncoding, latin1)
import Data.List (foldr1)
import Data.Map (unionWith, Map, keys, singleton)
import Data.Set (union, Set, singleton, empty)
import Data.Maybe (isNothing, fromJust)

import TypeClassesLib (WebPageInfoJson(..), InvertedIndex(..), decodeWebPageInfoJson, encodeInvertedIndex)

getLinks :: WebPageInfoJson -> T.Text
getLinks json = urlLink json

getWords :: WebPageInfoJson -> [(T.Text, Int)]
getWords json = textContentWords json

stripOccurence :: [[(a, b)]] -> [[a]]
stripOccurence = Prelude.map (Prelude.map fst)

flattenWords :: [[(a, b)]] -> [a]
flattenWords = concatMap (Prelude.map fst)

urlsForWord :: T.Text -> [(T.Text, [T.Text])] -> [T.Text]
urlsForWord word urlWordsTupleList = 
  let containsWord = Prelude.filter (\x -> elem word (snd x)) urlWordsTupleList
  in Prelude.map (\x -> (fst x)) containsWord

processOneIncoming :: (T.Text, [T.Text]) -> Map T.Text (Set T.Text)
processOneIncoming (link, wordsForLink) =
  let linksForWord = Prelude.map (\x -> Data.Map.singleton (x) (Data.Set.singleton link)) wordsForLink
  in if (null linksForWord)
    then 
      Data.Map.singleton link Data.Set.empty
    else 
      Data.Map.unionWith (Data.Set.union) (Data.Map.singleton link Data.Set.empty) (foldr1 (unionWith (Data.Set.union)) linksForWord)

createInvertedIndex :: IO ()
createInvertedIndex = do 
  jsonCollectionFile <- BB.readFile "webPageInfo.txt"
  let jsonList = Prelude.filter (not . BB.null) (BB.lines jsonCollectionFile)
  let jsonDecodedListMaybe = Prelude.map (decodeWebPageInfoJson) jsonList
  let jsonDecodedList = Prelude.map (fromJust) (Prelude.filter (not . isNothing) jsonDecodedListMaybe)

  let preparedJsonList = Prelude.map (\x -> ((urlLink x), Prelude.map (\y -> fst y) (textContentWords x))) jsonDecodedList
  let linksForWord = foldr1 (unionWith (Data.Set.union)) (Prelude.map (processOneIncoming) preparedJsonList)
  let invertedIndex = InvertedIndex linksForWord
  let invertedIndexEncoded = encodeInvertedIndex invertedIndex
  setLocaleEncoding latin1

  BB.writeFile "invertedIndex.txt" invertedIndexEncoded

  print "Finished creating inverted index!"