module Day2022_01 where

import Data.List.Split (splitOn)
import Data.List (sortBy)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/01.txt"
    -- part 1
    print $ maximum $ solve contents
    -- part 2
    print $ sum $ take 3 $ sortBy (flip compare) $ solve contents

solve :: String -> [Int]
solve = map (sum . map read . lines) . splitOn "\n\n"