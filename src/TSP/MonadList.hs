import Control.Monad

type Coord = (Int, Int)

jump :: Coord -> [Coord]
jump (x, y) = do
    x' <- [x+1, x-1, x+2, x-2]
    y' <- [y+1, y-1, y+2, y-2]
    guard (x' >= 0 && x' <= 7)
    guard (y' >= 0 && y' <= 7)
    guard (abs (x' - x) /= abs (y' - y))
    return (x', y')


-- jumpN : Coord -> Int -> [Coord]
-- jumpN

-- fromAtoB :: Coord -> Coord -> Bool
-- fromAtoB src dest
--     | src == dest = True
--     | src /= dest = case dest `elem` jumpOnceResult of
--         True -> True
--         False -> or $ map (\from -> fromAtoB from dest) jumpOnceResult
--     where   jumpOnceResult = jump src


solveConstraint :: [(Int, Int, Int)]
solveConstraint = do
  x <- choose [1,2,3]
  y <- choose [2,3,4]
  -- z <- [7, 8, 9]
  -- guard (x /= y)
  return (x, y, 1)
  where
  choose = id -- filter (> 2)
--
main= print solveConstraint