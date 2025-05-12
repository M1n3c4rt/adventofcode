module Day04 where

import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/04.txt"
    -- part 1
    print $ sum $ map (double . getWin . parseLine) $ lines contents
    -- part 2
    print $ sum $ processCards (repeat 1) $ map (getWin . parseLine) $ lines contents

parseLine :: String -> ([Int],[Int])
parseLine = (\[a,b] -> (a,b)) . map (map read . words) . splitOn "|" . tail . dropWhile (/=':')

getWin :: ([Int],[Int]) -> Int
getWin (w,y) = length $ filter (`elem` w) y

processCards :: [Int] -> [Int] -> [Int]
processCards (c:cards) (w:wins) = c:processCards (zipWith (+) (replicate w c ++ repeat 0) cards) wins
processCards cards [] = []

double :: Int -> Int
double n = floor $ 2**(fromIntegral n-1)