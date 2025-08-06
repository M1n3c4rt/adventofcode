module Day2016_03 where
import Data.List.Extra (chunksOf, transpose)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2016/03.txt"
    -- part 1
    print $ length $ filter isPossible $ map (map read . words) $ lines contents
    -- part 2
    print $ length $ concatMap (filter isPossible . transpose) (chunksOf 3 $ map (map read . words) $ lines contents)

isPossible :: [Int] -> Bool
isPossible [a,b,c] = a + b > c && b + c > a && a + c > b