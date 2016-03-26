module TSP.NN where

import TSP.Types
import TSP.Map

import qualified Data.List as L
import Data.List (filter)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, findMin, null, elems, lookup, empty)
import Prelude hiding (lookup, null, filter)


gainWtEdge :: Edge -> Map -> Map
gainWtEdge (E c1 c2 w) m = insertEdge (E c1 c2 (w+100)) $ deleteEdge (E c1 c2 w) m

loseWtEdge :: Edge -> Map -> Map
loseWtEdge (E c1 c2 w) m = insertEdge (E c1 c2 (w-100)) $ deleteEdge (E c1 c2 w) m


gainWtCity :: City -> Map -> Map
gainWtCity c m = gainWtEdges (incidentEdges c m) m
    where   gainWtEdges [] m = m
            gainWtEdges (e:es) m = gainWtEdges es $ gainWtEdge e m

loseWtCity :: City -> Map -> Map
loseWtCity c m = loseWtEdges (incidentEdges c m) m
    where   loseWtEdges [] m = m
            loseWtEdges (e:es) m = loseWtEdges es $ loseWtEdge e m

findMinWiBound :: City -> Map -> [Edge] -> [Edge]
findMinWiBound c m es =                             -- c is "the 'to City' of "the tail of es"".
    case findMin' c m of
        Just en -> findMinWiBound (to en) (gainWtCity c m) (es ++ [en])
        Nothing -> case ((==) 14 $ length es) of
                        True -> es
                        False -> findMinWiBound (from $ last es) (gainWtEdge (last es) $ loseWtCity (from $ last es) m) (init es)
tspNN :: City -> Map -> [Edge]
tspNN c m = findMinWiBound c m []
--
-- tspNN :: City -> Map -> [Edge]           -- will have prob: might not passthrough all City
-- tspNN c m = case findMin' c m of
--                 Nothing -> []       -- no "the City" in Map
--                 Just e ->            -- E cs (nearest City) w
--                     (:) e $ tspNN (to e) (deleteCity c m)


allBlock :: Map -> Bool
allBlock m = L.null $ filter (IntMap.null) $ map (IntMap.filterWithKey (\k _ -> k > 100) ) $ elems m

findMin' :: City -> Map -> Maybe Edge
findMin' c m = let result = edge c <$> findMin <$> lookupCity c m in
    case (>) 100 <$> weight <$> result of
                Just True -> result
                Just False -> Nothing
                Nothing -> Nothing
    where edge c (w, cs) = E c (head cs) w              -- A random pick when one meet two nearest path



--
-- -- lookup a city and returns a map of city-weight paires
lookupCity :: City -> Map -> Maybe (IntMap [Weight])
lookupCity c m = case lookup c m of
    Nothing -> Nothing
    Just a -> if a == empty then Nothing
        else Just a
