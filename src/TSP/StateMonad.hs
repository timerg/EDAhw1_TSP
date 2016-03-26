import Control.Monad.Writer
import Control.Monad.State
--
--
-- -- data Writer w a = Writer { runWriter :: (a, w) }
--
-- -- half ::  Int -> (Int, String)
-- -- half x = (x `div` 2, "I just halved " ++ (show x) ++ "!")
--
half :: Int -> Writer String Int
half x = do
        tell ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)
--
-- runWriter $ half 8
-- => (4, "I just halved 8!")


-- data Light = On | Off deriving (Show)

type Counter = Int

data TSPState = TSPState {
    cityMap ::
    }

calculateDist :: City -> City -> STatet TSPState Int
calculateDist a b = do
    m <- gets cityMap
    m 


tick :: State Counter ()
tick = modify succ


--
program :: State Counter ()
program = do
    tick
    tick
    tick
    tick
    tick