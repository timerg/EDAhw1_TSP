module TSP.Types where

import Data.IntMap hiding (map)
import qualified Data.IntMap as InM (map)
import Prelude hiding (lookup, null, filter)
import qualified Data.List as L
import Control.Monad
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Identity


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
type StepCount = Int
-- type Step = (Path, ToGo, StepCount)

data TSPState = TSPState
    {   bound :: Int
    ,   wMap :: WMap
    }

instance Show TSPState where
    show state = "Bound = " ++ (show $ bound state) ++ "\n" ++ (show $ wMap state)


type TSPM a = ListT (StateT TSPState IO) a

data Step = S
    {   stepPath :: Path
    ,   stepTogo :: ToGo
    ,   stepCount :: StepCount
    } deriving (Show)

instance Eq Step where
    (S _ _ sc1) == (S _ _ sc2) = sc1 == sc2

instance Ord Step where
    (S _ _ sc1) `compare` (S _ _ sc2) = sc1 `compare` sc2

data Result = Result [([Step], TSPState)]

instance Show Result where
    show (Result results) = L.intercalate "\n" (map show steps)
        where   steps = concat $ map fst results

type Dist = Int



-- singleton :: Key -> a -> IntMap a
-- insert :: Key -> a -> IntMap a -> IntMap a
-- insertWith :: (f: City -> City -> City)
            --  -> k : Key   假設剛開始 k 不存在，插入 k - x ； 否則，更新為 k - (f x 舊)
            --  -> x : [City]     要插入的值
            --  -> IntMap a  舊的
            --  -> IntMap a  新的結果


