module Day2016_13 where
import Utility.AOC (prettyPrintSet, neighbours4)
import qualified Data.Set as S
import Debug.Trace (traceShow)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/13.txt"
    -- part 1
    {-putStrLn $ unlines $ reverse $ lines $ prettyPrintSet $ S.fromList -}
    print $ floodFillGoal (read contents) S.empty (S.singleton (1,1)) (31,39)
    -- part 2
    print $ S.size $ floodFill (read contents) 50 S.empty (S.singleton (1,1))

isEmpty :: Int -> (Int,Int) -> Bool
isEmpty n (x,y) = even $ helper $ n + x*x + 3*x + 2*x*y + y + y*y
    
helper 0 = 0
helper k = fromEnum (odd k) + helper (div k 2)

inBounds :: (Int,Int) -> Bool
inBounds (x,y) = x >= 0 && y >= 0

floodFillGoal :: Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> (Int, Int) -> Int
floodFillGoal n finished frontier goal
    | goal `elem` frontier = 0
    | otherwise = succ $ floodFillGoal n (S.union frontier finished) (S.filter f $ S.unions $ S.map (S.fromList . neighbours4) frontier) goal
    where
        f x = inBounds x && x `S.notMember` finished && x `S.notMember` frontier && isEmpty n x

floodFill :: Int -> Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int,Int)
floodFill _ 0 finished frontier = S.union finished frontier
floodFill k n finished frontier = floodFill k (n-1) (S.union frontier finished) (S.unions $ S.map (S.fromList . ns) frontier)
    where
        ns f = filter (\x -> inBounds x && x `S.notMember` finished && x `notElem` frontier && isEmpty k x) $ neighbours4 f