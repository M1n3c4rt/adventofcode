module Day2025_05 where
import Data.List.Extra (splitOn)
import Utility.AOC (rangeIntersect)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2025/05.txt"
    let [a,b] = splitOn "\n\n" contents
    let (ranges,vals) = (map readRange (lines a), map read (lines b))
    -- part 1
    print $ length $ filter (\v -> any (inRange v) ranges) vals
    -- part 2
    print $ sum $ map rSize $ foldUnion [] ranges

readRange :: String -> (Int, Int)
readRange = (\[a,b] -> (read a,read b)) . splitOn "-"

inRange :: Int -> (Int, Int) -> Bool
inRange n (a,b) = n >= a && n <= b

subtractRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
subtractRange (a,b) (c,d) = case rangeIntersect (a,b) (c,d) of
    Nothing -> [(c,d)]
    Just (x,y) -> catMaybes [
            if y == b then Just (b+1,d) else Nothing,
            if x == a then Just (c,a-1) else Nothing
        ]

foldUnion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
foldUnion = foldl (\ acc r -> r : concatMap (subtractRange r) acc)

rSize :: (Int, Int) -> Int
rSize (a,b) = if b < a then 0 else b - a + 1