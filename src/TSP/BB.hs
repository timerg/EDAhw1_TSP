module TSP.BB where

import TSP.Types
import TSP.Map
import Data.List
import Data.IntMap (keys, empty)
import Prelude hiding (lookup, null, filter)
import Control.Monad
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Identity

----------------- BB --------------
-- Greedy Search : [[paths]]. where 'length [paths] = 15'




updateFoot :: City -> Foot -> Foot
updateFoot c (ps, ts) = ((ps ++ [c]), (filter (/= c) ts ))

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


runTSPM :: TSPM a -> TSPState -> IO ([a], TSPState)
runTSPM p s = runStateT (runListT p) s

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


selectFrom :: [a] -> TSPM a
selectFrom = ListT . return

formCycle' :: Step -> TSPM Step
formCycle' (S [] cs sc) = error "must have a starting city"
formCycle' (S (p:ps) (c:cs) sc) = do
    nextCity <- selectFrom (c:cs)   -- ListT (State Int) City;  nextCity :: City
    karte <- gets wMap
    guard (adjacent p nextCity karte)
    let nextToGo = filter (/= nextCity) (c:cs)
        nextPath = nextCity:p:ps
        nextStepCount = case (getWWeight p nextCity karte) of
            Nothing -> error "Impossible !!!"
            Just a -> sc + a
    bound <- gets bound
    -- lift $ lift $ print bound
    guard (getPathCost nextPath karte < bound)
    return (S nextPath nextToGo nextStepCount)
formCycle' (S (p:ps) [] sc) = do
    nextCity <- selectFrom [last (p:ps)]
    karte <- gets wMap
    guard (adjacent p nextCity karte)
    let nextPath = nextCity:p:ps
        nextStepCount = case (getWWeight p nextCity karte) of
            Nothing -> error "Impossible !!!"
            Just a -> sc + a
    theBound <- gets bound  :: TSPM Int
    guard (getPathCost nextPath karte < theBound)
    if nextStepCount < theBound
        then put $ TSPState nextStepCount karte
        else return ()
    return (S nextPath [] nextStepCount)

stopCycle :: Step -> Int -> Bool
stopCycle tms numberOfCities = (length $ pathOfStep tms) == numberOfCities + 1

pathOfStep :: Step -> Path
pathOfStep (S p t s) = p



-- Take intial step, find all the way to form a Cycle. Then return all results
findAllCycles :: Int -> Step -> TSPM Step
findAllCycles numberOfCities s = do
    if stopCycle s numberOfCities == True then
        return s
    else
        do
            result <- formCycle' s
            findAllCycles numberOfCities result
        -- formCycle' s >>= findAllCycles numberOfCities


genIntialStep :: [City] -> City -> Step
genIntialStep allCities initialCity = (S [initialCity] (delete initialCity allCities) 0)



------------ Search Cycles by taking each city as start point -> 15 Results of Step -----------

-- Get start from all city serially.
-- Ps. mapM will execuate serially.
runAllCities :: WMap -> [City] -> IO Result
runAllCities wm allCities = do
    result <- mapM (\initialCity -> leaveMin =<< runTSPM (findAllCycles (length allCities) (genIntialStep allCities initialCity)) (TSPState 100 wm)) allCities
    -- print (length result)
    -- print (map length result)
    -- print (take 1 result)
    print (Result result)
    return (Result result)

leaveMin :: ([Step], TSPState) -> IO ([Step], TSPState)
leaveMin ([], b) = do return ([], b)
leaveMin (ss, b) = do
    let minS = minimumStep ss
    return (minS, b)

minimumStep :: [Step] -> [Step]
minimumStep [] = []
minimumStep ss = [minimum ss]



------------ Search Cycles by taking one City as Start point -> 1 Result of Step -----------
runAllCities' :: WMap -> [City] -> IO Result
runAllCities' wm allCities = repeatRun allCities allCities (TSPState 100 wm)
    where   repeatRun :: [City] -> [City] -> TSPState -> IO Result
            repeatRun  allCities (c:cs) stateNow = do
                result <-  leaveMin =<< runTSPM (findAllCycles (length allCities) (genIntialStep allCities c)) stateNow
                let stateNext = takeResultBound result
                resultNext <- repeatRun allCities cs stateNext
                return $  addUpResult result resultNext
            repeatRun  allCities [] stateNow = do
                return $ Result [([], stateNow)]

addUpResult :: ([Step], TSPState) -> Result -> Result
addUpResult s (Result r) = Result (s:r)

takeResultBound :: ([Step], TSPState) -> TSPState
takeResultBound (ss, ts) = ts


----------------------------------



numberOfCycle :: Result -> Int
numberOfCycle (Result as) = sum . map (length . fst) $ as


----------------- For Testing --------------


mapWTest :: WMap
mapWTest = insertWEdge (E 1 4 3) $ insertWEdge (E 3 4 3) $ insertWEdge (E 1 2 3) $ insertWEdge (E 1 3 3) $ insertWEdge (E 3 2 3) $ insertWEdge (E 2 4 3) empty

-- runState will take a 'intial state' and 'value with empty state', then return compute result of ' new value' and 'final state'
-- p: m a; (ListT m a) -> m [a]
-- s == m;  State (s [a]) s -> (a, s)
-- runTSPM :: TSPM a -> TSPState -> ([a], TSPState)
-- runTSPM p s = runState (runListT p) s


stepTest :: Step
stepTest = (S [1] [3, 2, 4] 0)

tspStateIntial :: TSPState
tspStateIntial = (TSPState 100 mapWTest)

runTest :: IO ([Step], TSPState)
runTest = runTSPM (formCycle' stepTest) tspStateIntial

runTestMany = runTSPM (formCycle' stepTest >>= formCycle' >>= formCycle' >>= formCycle') tspStateIntial



-- testTspB = tspB mapWTest


--------------------------------------------









-- TSPM
-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-foot-monad
-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-writer-monad