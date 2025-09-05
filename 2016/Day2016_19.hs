module Day2016_19 where
import Utility.AOC (takeEvery)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/19.txt"
    -- part 1
    print $ cull True [1..read contents]
    -- part 2
    print $ weirdPattern !! read contents

cull :: Bool -> [Int] -> Int
cull _ [x] = x
cull lastParity l = cull (even $ length (if lastParity then l else tail l)) taken
    where
        taken = if lastParity then takeEvery 2 (undefined:l) else takeEvery 2 l

cull' :: Int -> [Int] -> Int
cull' 1 [x] = x
cull' n l = cull' (n-1) $ take (n-1) $ tail $ cycle $ filter (/=killed) l
    where killed = cycle l !! (n `div` 2)

weirdPattern :: [Int]
weirdPattern = [0,1,1,3] ++ concatMap (\n -> [1..3^n]++[3^n+2,3^n+4..3^(n+1)]) [1..]