module TSP.BB where

import TSP.Types
import TSP.Map
import qualified Data.List as L
import Prelude hiding (lookup, null, filter)
import Data.IntMap hiding (map)
import Control.Monad

----------------- BB --------------
-- Greedy Search : [[paths]]. where 'length [paths] = 15'



footTest :: Foot
footTest = ([1], [3, 2, 4])

updateFoot :: City -> Foot -> Foot
updateFoot c (ps, ts) = ((ps ++ [c]), (L.filter (/= c) ts ))

-- countDist :: [City] -> Map -> Dist
-- countDist (c1:(c2:cs)) m =
--
-- getWeight :: City -> City -> Map -> Weight
-- getWeight c1 c2 m = lookup c1 m

-- take a foot with [Citys] has passed and [Citys] to go; also take a map.
-- Start from the last City in 'paths' and find [City]: next possible citys through Map
-- If can't fnd City: 'Nothing', the start city isn't in Map -> error
-- Find difference between paths, keep the difference.
-- Will get original paths if no more new city to go
-- fahren :: Foot -> Map -> [Foot]
-- fahren (paths, ts) m = case  (L.\\) <$> lookupNextCity <*> (pure (init paths)) of
--     Nothing -> []       -- error
--     Just [] -> [(paths, ts)]
--     Just cs -> do
--         x <- cs
--         y <- (paths, ts):[]
--         return $ updateFoot x y
--     where
--         -- find the nextC of last City in paths, which will be a 'IntMap [City]'
--         -- elems thraw away keys, will get [[City]]; L.concat reduce it to [City]
--         lookupNextCity :: Maybe [City]
--         lookupNextCity = L.concat . elems <$> lookup (last paths) m



fahren :: Map -> Foot -> [Foot]
fahren karte (path  , [])     = return (path, [])
fahren karte ([]    , (c:cs)) = error "must have a starting city"
fahren karte ((p:ps), (c:cs)) = do
    nextCity <- c:cs
    guard (adjacent p nextCity)


    return ([], [])


-- fahren ::

-- tspBB :: Map -> [Foot]
-- tspBB m = do
--     st <- keys m








-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-foot-monad