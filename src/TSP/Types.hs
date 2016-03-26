module TSP.Types where

import Data.IntMap hiding (map)
import qualified Data.IntMap as InM (map)
import Prelude hiding (lookup, null, filter)
import qualified Data.List as L
import Control.Monad
type City = Int
type Weight = Int
type Cost = Int

data Edge = E
    { from      :: City
    , to        :: City
    , weight    :: Weight
    } deriving (Eq, Show, Ord)

-- City - Weight - City
type Map = IntMap (IntMap [City])
-- City - City - Weight
type WMap = IntMap (IntMap Weight)

data RandC = RC Cost [City]  deriving (Eq, Show)   -- cost and rout

type Path = [City]
type ToGo = [City]
type Foot = (Path, ToGo)
type Dist = Int

-- insertWEdge :: Edge -> Map -> Map
-- insertWEdge (E from to weight) m =
--     insertWith (\_ old -> insert to weight old) from (singleton to weight) $
--     insertWith (\_ old -> insert from weight old) to (singleton from weight) m

-- singleton :: Key -> a -> IntMap a
-- insert :: Key -> a -> IntMap a -> IntMap a
-- insertWith :: (f: City -> City -> City)
            --  -> k : Key   假設剛開始 k 不存在，插入 k - x ； 否則，更新為 k - (f x 舊)
            --  -> x : [City]     要插入的值
            --  -> IntMap a  舊的
            --  -> IntMap a  新的結果


