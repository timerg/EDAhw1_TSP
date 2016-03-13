module TSP.Types (insertEdge, lookupWeight, insertEdges, empty, Map, Edge(..)) where       -- Edge(..) to export everything in E

 import Data.IntMap
 import Prelude hiding (lookup)

 type City = Int
 -- type Edge = ((City, City), Weight)
 data Edge = E City City Weight deriving (Eq, Show)
 type Weight = Int
 type Map = IntMap (IntMap Weight)


 insertEdge :: Edge -> Map -> Map
 insertEdge (E from to weight) m =
     insertWith (\_ old -> insert to weight old) from (singleton to weight) $
     insertWith (\_ old -> insert from weight old) to (singleton from weight) m

 insertEdges :: [Edge] -> Map -> Map
 insertEdges [] m = m
 insertEdges (e:es) m =  insertEdges es (insertEdge e m)

 lookupWeight :: City -> City -> Map -> Maybe Weight
 lookupWeight from to m = lookup from m >>= lookup to

 -- findMinPath :: City -> Map -> Edge
 -- findMinPath c m =

 -- lookupWeight from to m = do
 --     result <- lookup from m
 --     lookup to result

 -- lookupWeight from to m = case lookup from m of
 --         Just a -> lookup to a
 --         Nothing -> Nothing
