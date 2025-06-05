module Day2023_05 where

import Data.List.Split (splitOn, chunksOf)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/05.txt"
    let (seeds,maps) = parseInput contents
    -- part 1
    print $ minimum $ map fst $ sliceRanges (getRanges1 seeds) maps
    -- part 2
    print $ minimum $ map fst $ sliceRanges (getRanges2 seeds) maps

type Map = [(Int,Int,Int,Int)]

parseInput :: String -> ([Int], [Map])
parseInput ss = let m:ms = splitOn "\n\n" ss in
    (map read . words . tail $ dropWhile (/=':') m,map (map ((\[a,b,c] -> (b,b+c,a,a+c)) . map read . words) . tail . lines) ms)

sliceRange :: Map -> (Int,Int) -> [(Int,Int)]
sliceRange ((p,q,r,s):map) (a,b)
    | p >= b || q <= a = sliceRange map (a,b)
    | a >= p && b <= q = [(a+offset,b+offset)]
    | a >= p && b > q = (a+offset,q+offset):sliceRange map (q,b)
    | a < p && b <= q = (p+offset,b+offset):sliceRange map (a,p)
    | a < p && b > q = (p+offset,q+offset):([(a,p),(q,b)] >>= sliceRange map)
    where offset = r-p
sliceRange [] (a,b) = [(a,b)]

sliceRanges :: [(Int,Int)] -> [Map] -> [(Int,Int)]
sliceRanges = foldl (\r m -> r >>= sliceRange m)

getRanges1 :: [Int] -> [(Int, Int)]
getRanges1 = map (\x -> (x,x+1))

getRanges2 :: [Int] -> [(Int, Int)]
getRanges2 = map (\[x,l] -> (x,x+l)) . chunksOf 2