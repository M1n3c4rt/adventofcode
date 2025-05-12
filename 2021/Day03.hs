module Day03 where

import Data.List (transpose, sort, group, maximumBy, minimumBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing, Down (Down))

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2021/03.txt"
    let p = parse contents
    let p' = transpose p
    -- part 1
    print $ binToDec (map mode p') * binToDec (map unmode p')
    -- part 2
    print $ binToDec (filterCO2 p 0) * binToDec (filterOxygen p 0)

parse :: String -> [[Int]]
parse = map (map (read . return)) . lines

mode :: Ord a => [a] -> a
mode = head . maximumBy (compare `on` length) . group . sort

unmode :: Ord a => [a] -> a
unmode = head . minimumBy (compare `on` length) . group . sort

binToDec :: [Int] -> Int
binToDec = sum . zipWith (*) (map (2^) [0..]) . reverse

filterOxygen :: Ord a => [[a]] -> Int -> [a]
filterOxygen ns i =
    let m = mode (map (!!i) ns)
        new = filter ((==m) . (!!i)) ns
    in case new of
        [a] -> a
        as -> filterOxygen as (i+1)

filterCO2 :: Ord a => [[a]] -> Int -> [a]
filterCO2 ns i =
    let m = unmode (map (!!i) ns)
        new = filter ((==m) . (!!i)) ns
    in case new of
        [a] -> a
        as -> filterCO2 as (i+1)