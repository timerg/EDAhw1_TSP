{-# LANGUAGE OverloadedStrings #-}

module Main where

import TSP.Types
import TSP.Parser
import TSP.Map

import System.Environment
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, readFile)
import Data.IntMap hiding (null, map)
import qualified Data.IntMap as InM (map)
import Data.Either
import Data.List (intercalate)
import Prelude hiding (readFile)
import TSP.BB
import TSP.NN


run :: String -> IO ()
run path = do
    input <- readFile path
    case parseFile' input of
        Nothing    -> print "parse Failed"
        Just edges -> do
            let karte = buildMap edges
            let karteW = buildWMap edges
            resultBest <- runAllCities' karteW (keys karteW)
            resultAllCities <- runAllCities karteW (keys karteW)
            putStrLn $ (show $ numberOfCycle resultBest) ++ ("\n") ++ (show resultBest)
            -- writeFile "./data/result_NN.txt" $ intercalate "\n" $ map (show.(tspNN karte)) (keys karte)
            writeFile "./data/result_BB.txt" $ (show $ numberOfCycle resultBest) ++ ("\n") ++ (show resultBest)
            -- writeFile "./data/result_AllCities.txt" $ (show $ numberOfCycle resultAllCities) ++ ("\n") ++ (show resultAllCities)



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

