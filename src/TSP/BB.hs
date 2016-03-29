module TSP.BB where

import TSP.Types
import TSP.Map
import Data.List
import Data.IntMap (keys)
import Prelude hiding (lookup, null, filter)
import Control.Monad
import Control.Monad.State
import Control.Monad.List

----------------- BB --------------
-- Greedy Search : [[paths]]. where 'length [paths] = 15'




updateFoot :: City -> Foot -> Foot
updateFoot c (ps, ts) = ((ps ++ [c]), (filter (/= c) ts ))

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



formCycle :: WMap -> Foot -> [Foot]
formCycle karte ([]    , cs) = error "must have a starting city"
formCycle karte ((p:ps), (c:cs)) = do
    nextCity <- c:cs
    guard (adjacent p nextCity karte)
    return (nextCity:p:ps, filter (/= nextCity) (c:cs))
formCycle karte (p:ps  , [])     = do
    nextCity <- [last (p:ps)]
    guard (adjacent p nextCity karte)
    return (nextCity:p:ps, [])


data TSPState = TSPState
    {   bound :: Int
    ,   wMap :: WMap
    } deriving (Show)

type TSPM a = ListT (State TSPState) a

selectFrom :: [a] -> TSPM a
selectFrom = ListT . return

formCycle' :: Step -> TSPM Step
formCycle' ([]    , cs, sc) = error "must have a starting city"
formCycle' ((p:ps), (c:cs), sc) = do
    nextCity <- selectFrom (c:cs)   -- ListT (State Int) City;  nextCity :: City
    karte <- gets wMap
    guard (adjacent p nextCity karte)
    let nextToGo = filter (/= nextCity) (c:cs)
        nextPath = nextCity:p:ps
        nextStepCount = case (getWWeight p nextCity karte) of
            Nothing -> error "Impossible !!!"
            Just a -> sc + a
    bound <- gets bound
    guard (getPathCost nextPath karte < bound)
    return (nextPath, nextToGo, nextStepCount)
formCycle' (p:ps  , [], sc) = do
    nextCity <- selectFrom [last (p:ps)]
    karte <- gets wMap
    guard (adjacent p nextCity karte)
    let nextPath = nextCity:p:ps
        nextStepCount = case (getWWeight p nextCity karte) of
            Nothing -> error "Impossible !!!"
            Just a -> sc + a
    bound <- gets bound  :: TSPM Int
    guard (getPathCost nextPath karte < bound)
    return (nextPath, [], nextStepCount)

stopCycle :: Step -> TSPM Bool
stopCycle tms = do
    return $ (length $ pathOfStep tms) == 16

pathOfStep :: Step -> Path
pathOfStep (p, t, s) = p


recursFormCycle :: TSPM Step -> TSPM Step
recursFormCycle s = do
     s1 <- s
     s2 <- formCycle' s1  :: TSPM Step
     stopresult <- stopCycle s2
     if stopresult == True
         then return s2
     else
         recursFormCycle (formCycle' s1)

tspBB :: WMap -> ([Step], TSPState)
tspBB wm = let (c:cs) = keys (wm)
    in  runTSPM ((recursFormCycle.formCycle') ([c], cs, 0)) (TSPState 100 wm)
                -- (recursFormCycle $ formCycle' ...)   this will get wrong result. Why?

tspmLength :: ([Step], TSPState) -> Int
tspmLength (a, b) = length $ fst (a, b)




----------------- For Testing --------------

-- runState will take a 'intial state' and 'value with empty state', then return compute result of ' new value' and 'final state'
-- p: m a; (ListT m a) -> m [a]
-- s == m;  State (s [a]) s -> (a, s)
runTSPM :: TSPM a -> TSPState -> ([a], TSPState)
runTSPM p s = runState (runListT p) s

stepTest :: Step
stepTest = ([1], [3, 2, 4], 0)

tspStateIntial :: TSPState
tspStateIntial = (TSPState 100 mapWTest)

runTest :: ([Step], TSPState)
runTest = runTSPM (formCycle' stepTest) tspStateIntial

runTestMany = runTSPM (formCycle' stepTest >>= formCycle' >>= formCycle' >>= formCycle') tspStateIntial

-- test :: (([Bool], TSPState))
-- test = runTSPM (f ([1,2], [3, 4], 0)) tspStateIntial
--     where f s = do              --TSPM Bool
--             s2 <- formCycle' s
--             stopCycle s2



--------------------------------------------










-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-foot-monad
-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-writer-monad