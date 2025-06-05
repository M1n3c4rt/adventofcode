module Day2022_04 where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/04.txt"
    -- part 1
    print $ length $ filter (\(x,y) -> intersect x y `elem` [Just x,Just y]) $ parse contents
    -- part 2
    print $ length $ mapMaybe (uncurry intersect) $ parse contents

type Range = (Int,Int)

parse :: String -> [(Range, Range)]
parse = map ((\[[a,b],[c,d]] -> ((a,b),(c,d))) . map (map read . splitOn "-") . splitOn ",") . lines

intersect :: Range -> Range -> Maybe Range
intersect (a,b) (c,d)
    | b < c || d < a = Nothing
    | otherwise = Just (max a c, min b d)