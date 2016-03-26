{-# LANGUAGE OverloadedStrings #-}

module TSP.Parser where

import TSP.Types(insertEdgew, insertEdges, Map, Edge(..))

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

-- parseMap :: Parser Map
-- parseMap = do
--     result <- insertEdges <$> many1 parseEdge <*> pure empty
--
--     endOfInput
--     return result
parseMap :: Parser [Edge]
parseMap = many1 parseEdge

parseFile :: Parser [Edge]
parseFile = do
    scientific
    endOfLine
    many1 parseEdge

buildMap :: [Edge] -> Map
buildMap [] = empty
buildMap (e:es) = insertEdgew e $ buildMap es

    -- skipWhile (\c -> c == '\r' || c == '\n' || isSpace c)
    -- return result
