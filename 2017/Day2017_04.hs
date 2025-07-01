module Day2017_04 where
import Data.List (nub, sort)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2017/04.txt"
    -- part 1
    print $ length $ filter (\x -> length (nub x) == length x) $ map words $ lines contents
    -- part 2
    print $ length $ filter (\x -> length (nub x) == length x) $ map (map sort . words) $ lines contents
