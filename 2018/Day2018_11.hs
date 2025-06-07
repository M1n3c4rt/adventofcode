module Day2018_11 where
import qualified Data.HashMap.Strict as HM
import Data.Foldable (maximumBy)
import Data.Function (on)
import Debug.Trace (traceShowId)
import Data.MemoUgly (memo)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2018/11.txt"
    let (t::Int) = read $ head $ lines contents
    -- part 1
    putStrLn $ init $ tail $ show $ fst $ maximumBy (compare `on` snd) $ genWindows t
    -- part 2
    putStrLn $ init $ tail $ show $ fst $ maximumBy (compare `on` snd) $ genWindows' t

genGrid t = [((x,y),((a^2*y+a*t) `mod` 1000) `div` 100) | x <- [1 .. 300], let a = x + 10, y <- [1 .. 300]]

genWindows t = [((x,y),windowMemo (t,(3,3),(x,y))) | x <- [1 .. 298], y <- [1 .. 298]]

genWindows' t = [((x,y,n),windowMemo (t,traceShowId (n,n),(x,y))) | n <- [1..300], x <- [1 .. 300-(n-1)], y <- [1 .. 300-(n-1)]]

window (t,(m,n),(x,y))
    | m == 0 || n == 0 = 0
    | (m,n) == (1,1) = (((a^2*y+a*t) `mod` 1000) `div` 100) - 5
    | otherwise = sum [
        windowMemo (t,(m `div` 2, n `div` 2),(x,y)),
        windowMemo (t,(m - (m `div` 2), n `div` 2),(x + m `div` 2,y)),
        windowMemo (t,(m `div` 2, n - (n `div` 2)),(x,y + n `div` 2)),
        windowMemo (t,(m - (m `div` 2), n - (n `div` 2)),(x + m `div` 2,y + n `div` 2))
    ]
    where a = x + 10

windowMemo = memo window