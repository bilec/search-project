module Main where

import ParseLib
import PageRankLib

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
        "1" -> parse
        "2" -> pageRank
        "3" -> print "3 selected"
        "4" -> print "4 selected"
        otherwise -> print "wrong option, try again"
    menu
