module Main where

import System.Environment
import TSP.Types



run :: String -> IO ()
run path = do
    content <- readFile path
    putStrLn content

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
