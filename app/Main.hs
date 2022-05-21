module Main where

import Data.Time (getCurrentTime, diffUTCTime)

import ParseLib
import PageRankLib
import InvertedIndexLib
import SearchLib

main :: IO ()
main = menu

menu :: IO ()
menu = do
    print "Menu:"
    print "1. Parse collection.jl file"
    print "2. Calculate pageRank"
    print "3. Calculate reverse index"
    print "4. Search"

    userInput <- getLine
    case userInput of 
        "1" -> timeFunction parse
        "2" -> timeFunction pageRank
        "3" -> timeFunction createInvertedIndex
        "4" -> timeFunction search
        otherwise -> print "wrong option, try again"
    menu

timeFunction :: IO () -> IO ()
timeFunction functionToTime = do
    start <- getCurrentTime
    functionToTime
    end <- getCurrentTime
    print (diffUTCTime end start)