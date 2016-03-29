import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.List
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



--     data TSPState = TSPState {
--     -- cityMap ::
-- }
--
-- calculateDist :: City -> City -> STatet TSPState Int
-- calculateDist a b = do
--     m <- gets cityMap
--     m





-- runStateT :: StateT s m a -> s -> m (a, s)
--      s : State
--      m : Inner Monad Stack
--      a : Computation


calculate :: Int -> [Int]
calculate n = [n + 2, n * 2]

race :: StateT Bool [] String
race = do
    n <- lift [1..100]

    if n > 10 then
        put True
    else
        put False


    return "hi"


globalRace :: ListT (State Bool) Int
globalRace = do -- in List Monad

    n <- (ListT . return) [1..5]
    guard (n < 5)

    if n > 3 then
        lift (put True)
    else
        lift (put False)


    -- n <- ListT $ do -- in State Monad
    --     put True
    --     return [1 .. 3]
    return n
    -- return n

