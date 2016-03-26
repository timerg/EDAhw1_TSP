{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import TSP.Types
import TSP.Parser
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Data.IntMap hiding (null, map)
import qualified Data.IntMap as InM (map)
import Data.Either



getRandC_NN :: [Edge] -> RandC
getRandC_NN [] = RC 0 []
getRandC_NN (e:es) = RC (tspCost (e:es)) (map from (e:es))

run :: String -> IO ()
run path = do
    input <- pack <$> readFile path
    let content = parseOnly (buildMap <$> parseFile) input      -- <- only extract stuff in "IO"    when its in do block of IO()
    case content of
        Left s -> print "parse Failed"
        Right m -> print $ getRandC_NN $ tspNN 1 m

    -- print $ content
    -- putStrLn input




test :: IO ()
-- test = run "data/hw1.TSP.txt"
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
