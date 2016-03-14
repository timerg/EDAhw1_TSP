{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import TSP.Types
import TSP.Parser
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)


run :: String -> IO ()
run path = do
    content <- readFile path
    print $ parseOnly parseFile $ pack content
    -- print content

test :: IO ()
test = run "data/hw1.TSP.txt"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then
            putStrLn "Please supply path"
        else
            run (head args)
            -- do
            --
            --     content <- readFile $ head args
            --     putStrLn content
