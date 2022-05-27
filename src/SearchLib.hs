{-# LANGUAGE OverloadedStrings #-}

module SearchLib (
  search
) where
        
import qualified Data.ByteString.Lazy.Char8 as BB
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Map (Map, map, member)
import Data.Set (Set, map, member)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Maybe (isNothing, fromJust)

import TypeClassesLib (InvertedIndex(..), PageRankJson(..), decodeInvertedIndex, decodePageRankJson)

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
    let pageRankDecodedAndParsedList = Prelude.map (\x -> ((prUrl x), (pageRankValue x))) pageRankDecodedList
    let pageRankMap = M.fromList pageRankDecodedAndParsedList
    
    print "Please insert text to search:"
    userInput <- getLine
    let invIndex = index (fromJust invIndexJson)
    if M.member (pack userInput) invIndex 
        then print $ sortLinks (findWord (pack userInput) invIndex) pageRankMap
        else print "No result"