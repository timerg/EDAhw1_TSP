{-# LANGUAGE OverloadedStrings #-}

module TSP.Parser where

import TSP.Types

import Data.Attoparsec.ByteString.Char8
-- import Data.ByteString.Char8 (ByteString)
import Data.Scientific (Scientific, coefficient)
import Data.IntMap




parseEdge :: Parser Edge
parseEdge = do
    char '('
    skipSpace
    from <- fromInteger . coefficient <$> scientific
    skipSpace
    char ','
    skipSpace
    to <- fromInteger . coefficient <$>scientific
    skipSpace
    char ')'
    skipSpace
    weight <- fromInteger . coefficient <$>scientific
    skipSpace
    return $ E from to weight

-- test :: ByteString
-- test = "3\n(1, 2) 0\n(3, 4) 0\n(5, 6) 0\n"

parseMap :: Parser Map
parseMap = do
    insertEdges <$> many1 parseEdge <*> pure empty

parseFile :: Parser Map
parseFile = do
    scientific
    endOfLine
    result <- parseMap

    -- skipWhile (\c -> c == '\r' || c == '\n' || isSpace c)
    return result
