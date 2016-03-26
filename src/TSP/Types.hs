module TSP.Types (tspCost, tspNN, insertEdgew, insertEdges, Map, Edge(..), RandC(..), deleteCity, City)        -- Edge(..) to export everything in E
    where
import Data.IntMap hiding (map)
import qualified Data.IntMap as InM (map)
import Prelude hiding (lookup, null, filter)
import qualified Data.List as L
import Control.Monad
type City = Int
type Weight = Int
type Cost = Int

-- type Edge = ((City, City), Weight)

-- data Edge = E City City Weight deriving (Eq, Show)
-- from (E c1 c2 w) = c1
-- to (E c1 c2 w) = c2
-- weight (E c1 c2 w) = w

data Edge = E { from :: City
              , to :: City
              , weight:: Weight
              } deriving (Eq, Show, Ord)


type Map = IntMap (IntMap [City])

-- City: Taipei : { 60km : [宜蘭, 新竹],  80km: .... }
-- Weight: 60km



data RandC = RC Cost [City]  deriving (Eq, Show)   -- cost and rout


mapTest :: Map
mapTest = insertEdgew (E 1 4 6) $ insertEdgew (E 3 4 5) $ insertEdgew (E 1 2 3) $ insertEdgew (E 1 3 3) $ insertEdgew (E 3 2 1) empty
--
-- insertEdge :: Edge -> Map -> Map
-- insertEdge (E from to weight) m =
--     insertWith (\_ old -> insert to weight old) from (singleton to weight) $
--     insertWith (\_ old -> insert from weight old) to (singleton from weight) m
-- -- singleton :: Key -> a -> IntMap a
-- -- insert :: Key -> a -> IntMap a -> IntMap a
-- -- insertWith :: (f: City -> City -> City)
--              -> k : Key   假設剛開始 k 不存在，插入 k - x ； 否則，更新為 k - (f x 舊)
--              -> x : [City]     要插入的值
--              -> IntMap a  舊的
--              -> IntMap a  新的結果
--
insertEdgew :: Edge -> Map -> Map       -- use weight as key(inside)
insertEdgew (E from to weight) m =
    insertWith (\_ old -> insertWith nubcombine weight [to] old) from (singleton weight [to]) $
    insertWith (\_ old -> insertWith nubcombine weight [from] old) to (singleton weight [from]) m
    where   nubcombine as bs = L.nub (as ++ bs)





insertEdges :: [Edge] -> Map -> Map
insertEdges [] m = m
insertEdges (e:es) m =  insertEdges es (insertEdgew e m)

-- lookup :: Key -> IntMap a -> Maybe a
-- lookupWeight :: City -> City -> Map -> Maybe Weight
-- lookupWeight from to m = lookup from m >>= lookup to
--
-- keys :: IntMap a -> [Key]
-- elems :: IntMap a -> [a]
-- assocs :: IntMap a -> [(Key, a)]
assocs' :: City -> Map -> [Edge]
assocs' c m = case lookup c m of
                Nothing -> []
                Just m2 -> concat $ map (plug c) $ assocs m2

plug :: City -> (Weight, [City]) -> [Edge]
plug c (w, (c2:[])) = [E c c2 w]
plug c (w, (c2:cs)) = [E c c2 w] ++ (plug c (w, cs))


deleteCity :: City -> Map -> Map
deleteCity c m = filter (not.null) $ deletes (assocs' c m) m
    where   deletes [] m = m
            deletes (e:es) m = deletes es $ deleteEdge e m

gainWtCity :: City -> Map -> Map
gainWtCity c m = gainWtEdges (assocs' c m) m
    where   gainWtEdges [] m = m
            gainWtEdges (e:es) m = gainWtEdges es $ gainWtEdge e m

loseWtCity :: City -> Map -> Map
loseWtCity c m = loseWtEdges (assocs' c m) m
    where   loseWtEdges [] m = m
            loseWtEdges (e:es) m = loseWtEdges es $ loseWtEdge e m


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

gainWtEdge :: Edge -> Map -> Map
gainWtEdge (E c1 c2 w) m = insertEdgew (E c1 c2 (w+100)) $ deleteEdge (E c1 c2 w) m

loseWtEdge :: Edge -> Map -> Map
loseWtEdge (E c1 c2 w) m = insertEdgew (E c1 c2 (w-100)) $ deleteEdge (E c1 c2 w) m




-- deleteCityList :: [City] -> Map -> Map
-- deleteCityList [] m = m
-- deleteCityList (c:cs) m = deleteCityList cs $ deleteCity c m


--
-- -- lookup a city and returns a map of city-weight paires
lookupCity :: City -> Map -> Maybe (IntMap [Weight])
lookupCity c m = case lookup c m of
    Nothing -> Nothing
    Just a -> if a == empty then Nothing
        else Just a
--
--
findMin' :: City -> Map -> Maybe Edge
findMin' c m = let result = edge c <$> findMin <$> lookupCity c m in
    case (>) 100 <$> weight <$> result of
                Just True -> result
                Just False -> Nothing
                Nothing -> Nothing
    where edge c (w, cs) = E c (head cs) w              -- A random pick when one meet two nearest path
--
-- tspNN :: City -> Map -> [Edge]           -- will have prob: might not passthrough all City
-- tspNN c m = case findMin' c m of
--                 Nothing -> []       -- no "the City" in Map
--                 Just e ->            -- E cs (nearest City) w
--                     (:) e $ tspNN (to e) (deleteCity c m)

tspNN :: City -> Map -> [Edge]
tspNN c m = findMinWiBound c m []

findMinWiBound :: City -> Map -> [Edge] -> [Edge]
findMinWiBound c m es =                             -- c is "the 'to City' of "the tail of es"".
    case findMin' c m of
        Just en -> findMinWiBound (to en) (gainWtCity c m) (es ++ [en])
        Nothing -> case ((==) 14 $ length es) of
                        True -> es
                        False -> findMinWiBound (from $ last es) (gainWtEdge (last es) $ loseWtCity (from $ last es) m) (init es)

allBlock :: Map -> Bool
allBlock m = L.null $ L.filter (null) $ map (filterWithKey (\k _ -> k > 100) ) $ elems m



tspCost :: [Edge] -> Cost
tspCost e = sum (map weight e)
--
--
--
-- -- lookup :: Key -> IntMap a -> Maybe a
-- -- findMin :: IntMap a -> (Key, a)
--
-- -- lookupWeight from to m = lookup from m >>= lookup to
--
--
-- -- lookupWeight from to m = do
-- --     result <- lookup from m
-- --     lookup to result
--
-- -- lookupWeight from to m = case lookup from m of
-- --         Just a -> lookup a
-- --         Nothing -> Nothing


----------------- BB --------------
-- Greedy Search : [[paths]]. where 'length [paths] = 15'

type Path = [City]
type ToGo = [City]
type Foot = (Path, ToGo)
type Dist = Int

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
fahren :: Foot -> Map -> [Foot]
fahren (paths, ts) m = case  (L.\\) <$> lookupNextCity <*> (pure (init paths)) of
    Nothing -> []       -- error
    Just [] -> [(paths, ts)]
    Just cs -> do
        x <- cs
        y <- (paths, ts):[]
        return $ updateFoot x y
    where
        -- find the nextC of last City in paths, which will be a 'IntMap [City]'
        -- elems thraw away keys, will get [[City]]; L.concat reduce it to [City]
        lookupNextCity :: Maybe [City]
        lookupNextCity = L.concat . elems <$> lookup (last paths) m

-- tspBB :: Map -> [Foot]
-- tspBB m = do
--     st <- keys m








-- http://adit.io/posts/2013-06-10-three-useful-monads.html#the-foot-monad