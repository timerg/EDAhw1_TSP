module TSP.Map where

import TSP.Types

import Data.IntMap hiding (map, foldr)
import qualified Data.List as L
import Prelude hiding (lookup, null, filter)


mapTest :: Map
mapTest = insertEdge (E 1 4 6) $ insertEdge (E 3 4 5) $ insertEdge (E 1 2 3) $ insertEdge (E 1 3 3) $ insertEdge (E 3 2 1) empty

mapWTest :: WMap
mapWTest = insertWEdge (E 1 4 6) $ insertWEdge (E 3 4 5) $ insertWEdge (E 1 2 3) $ insertWEdge (E 1 3 3) $ insertWEdge (E 3 2 1) empty

--------------------------------------------------------------------------------
--  City - Weight - City
--------------------------------------------------------------------------------

insertEdge :: Edge -> Map -> Map       -- use weight as key(inside)
insertEdge (E from to weight) m =
    insertWith (\_ old -> insertWith nubCombine weight [to] old) from (singleton weight [to]) $
    insertWith (\_ old -> insertWith nubCombine weight [from] old) to (singleton weight [from]) m
    where   nubCombine as bs = L.nub (as ++ bs)

buildMap :: [Edge] -> Map
buildMap [] = empty
buildMap (e:es) = insertEdge e $ buildMap es



--------------------------------------------------------------------------------
--  City - City - Weight
--------------------------------------------------------------------------------

insertWEdge :: Edge -> WMap -> WMap
insertWEdge (E from to weight) m =
    insertWith (\_ old -> insert to weight old)   from (singleton to weight) $
    insertWith (\_ old -> insert from weight old) to   (singleton from weight) $ m

-- https://www.google.com.tw/search?q=fold+haskell&safe=off&espv=2&biw=1280&bih=705&source=lnms&tbm=isch&sa=X&ved=0ahUKEwjhrpbq6t7LAhUHX5QKHfy_AnYQ_AUICCgD#imgrc=lIlTirQq1smk4M%3A
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
buildWMap :: [Edge] -> WMap
buildWMap es = foldr insertWEdge empty es

getWWeight :: City -> City -> WMap -> Maybe Weight
getWWeight c1 c2 wm = do
    lookup c1 wm >>= lookup c2

getPathCost :: Path -> WMap -> Cost
getPathCost (c1:c2:cs) wm = case getWWeight c1 c2 wm of
    Nothing -> error "No path for some Citys in Path"
    Just w -> (+) w $ getPathCost (c2:cs) wm
getPathCost cs wm = 0

adjacent :: City -> City -> WMap -> Bool
adjacent a b m = case lookup a m of
    Nothing -> False
    Just m' -> member b m'








-- get all of the edges going out from a city
incidentEdges :: City -> Map -> [Edge]
incidentEdges c m = case lookup c m of
    Nothing -> []
    Just m2 -> concat $ map (plug c) $ assocs m2
    where
            plug :: City -> (Weight, [City]) -> [Edge]
            plug c (w, (c2:[])) = [E c c2 w]
            plug c (w, (c2:cs)) = [E c c2 w] ++ (plug c (w, cs))


deleteCity :: City -> Map -> Map
deleteCity c m = filter (not.null) $ deletes (incidentEdges c m) m
    where   deletes [] m = m
            deletes (e:es) m = deletes es $ deleteEdge e m

deleteEdge :: Edge -> Map -> Map    -- lookup c1 -> delete Edge from c1's Map than add it back
deleteEdge (E c1 c2 w) m =
    let mx = case lookup c1 m of
            Nothing -> m
            Just mcs -> insert c1 (deleteCity2 w c2 mcs) m
    in case lookup c2 mx of
            Nothing -> mx
            Just mcs2 -> insert c2 (deleteCity2 w c1 mcs2) mx

deleteCity2 :: Weight -> City -> (IntMap [City]) -> (IntMap [City])
deleteCity2 w c m = case lookup w m of
    Nothing -> m
    -- Just cs -> insert w (L.filter (/= c) cs) m
    Just cs ->
        let new = (L.filter (/= c) cs)
        in case L.null new of True -> delete w m
                              False -> insert w new m
