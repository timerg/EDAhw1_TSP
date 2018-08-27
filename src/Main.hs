{-# LANGUAGE OverloadedStrings #-}

module Main where

import TSP.Types
import TSP.Parser
import TSP.Map
import TSP.BB
import TSP.NN

import System.Environment
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, readFile)
import Data.IntMap hiding (null, map)
import qualified Data.IntMap as InM (map)
import Data.Either
import Data.List (intercalate)
import Prelude hiding (readFile)


data TestT = NN | All | Normal

run :: TestT -> String -> IO ()
run testType path = do
    input <- readFile path
    case parseFile' input of
        Nothing    -> print "parse Failed"
        Just cityEdges -> do
            let karte = buildMap cityEdges
            let karteW = buildWMap cityEdges
            Result results <- runAllCities' karteW (keys karteW)
            resultAllCities <- runAllCities karteW (keys karteW)
            let step = head $ fst $ head results
            let bestCycle = stepPath step
            let totalLength = stepCount step
            case testType of 
                All -> putStrLn $ (show resultAllCities)
                NN -> putStrLn $ intercalate "\n" $ (map (show.printComplete.(tspNN karte)) (keys karte))
                    where checkComplete es = length (edges es) >= length (keys karteW)
                          printComplete es = case checkComplete es of
                                                True ->  "Travel complete, " ++ (show es)
                                                False -> "Travel not complete, " ++ (show es)
                Normal -> putStrLn $ "total length: " ++ show totalLength ++
                    "cycle: " ++ show bestCycle



test :: IO ()
test = run NN "data/hw1.TSP.txt"



main :: IO ()
main = do
    args <- getArgs
    if null args
        then
            putStrLn "Please supply path"
        else
            let a1:xs = args in case xs of 
                p:[] -> case a1 of 
                    "All" -> run All p
                    "NN" -> run NN p
                [] -> run Normal a1
                s:xxs -> putStrLn "Too many arguments"
