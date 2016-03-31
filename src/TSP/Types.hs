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

data Edges = Es
    {edges :: [Edge]}
instance Show Edges where
    show (Es es) = "The Cost is " ++ (show $ L.sum $ map weight es) ++ ",\t"
        ++ (show $ (length.edges) (Es es)) ++ " cities has passed."
        ++ "\t" ++ (show $ L.map from (es))


-- City - Weight - City
type Map = IntMap (IntMap [City])
-- City - City - Weight
type WMap = IntMap (IntMap Weight)


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





