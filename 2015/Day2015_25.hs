module Day2015_25 where
import Utility.AOC (numbers)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2015/25.txt"
    -- part 1
    print $ (!!gridToN (reverse $ numbers contents)) $ iterate next 20151125
    -- part 2
    -- gg!

gridToN :: [Int] -> Int
gridToN [x,y] = bar*(bar+1) `div` 2 + pos
    where
        bar = x + y - 2
        pos = x - 1

next n = (n * 252533) `mod` 33554393