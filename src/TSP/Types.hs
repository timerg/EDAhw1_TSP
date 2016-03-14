module TSP.Types (insertEdge, lookupWeight, insertEdges, empty, Map, Edge(..), findMin') where       -- Edge(..) to export everything in E

import Data.IntMap
import Prelude hiding (lookup)

type City = Int
type Weight = Int
-- type Edge = ((City, City), Weight)
data Edge = E City City Weight deriving (Eq, Show)
type Map = IntMap (IntMap Weight)

mapTest :: Map
mapTest = insertEdge' (E 1 2 3) $ insertEdge' (E 1 3 4) $ insertEdge' (E 3 2 1) empty

insertEdge :: Edge -> Map -> Map
insertEdge (E from to weight) m =
    insertWith (\_ old -> insert to weight old) from (singleton to weight) $
    insertWith (\_ old -> insert from weight old) to (singleton from weight) m
-- singleton :: Key -> a -> IntMap a
-- insert :: Key -> a -> IntMap a -> IntMap a
-- insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a

insertEdge' :: Edge -> Map -> Map       -- use weight as key(inside)
insertEdge' (E from to weight) m =
    insertWith (\_ old -> insert weight to old) from (singleton weight to) $
    insertWith (\_ old -> insert weight from old) to (singleton weight from) m

insertEdges :: [Edge] -> Map -> Map
insertEdges [] m = m
insertEdges (e:es) m =  insertEdges es (insertEdge' e m)

lookupWeight :: City -> City -> Map -> Maybe Weight
lookupWeight from to m = lookup from m >>= lookup to

findMin' :: City -> Map -> Maybe Edge
findMin' c m = edge c <$> findMin <$> lookup c m
    where edge c (w, c2) = E c c2 w
-- lookup :: Key -> IntMap a -> Maybe a
-- findMin :: IntMap a -> (Key, a)

-- lookupWeight from to m = do
--     result <- lookup from m
--     lookup to result

-- lookupWeight from to m = case lookup from m of
--         Just a -> lookup to a
--         Nothing -> Nothing
