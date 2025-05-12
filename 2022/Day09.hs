{-# OPTIONS_GHC -Wno-missing-methods #-}
module Day09 where

import Data.List (nub)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/09.txt"
    let dirs = parse contents
    -- part 1
    print $ length $ nub $ move (replicate 2 (0,0)) dirs
    -- part 2
    print $ length $ nub $ move (replicate 10 (0,0)) dirs
    --mapM_ print $ move' (replicate 10 (0,0)) dirs

parse :: String -> [(Int, Int)]
parse = concatMap helper . lines
    where helper (a:_:cs) = case a of
            'U' -> replicate (read cs) (0, 1)
            'D' -> replicate (read cs) (0,-1)
            'L' -> replicate (read cs) (-1,0)
            'R' -> replicate (read cs) ( 1,0)

move :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
move rope (d:dirs) = let newrope = submove rope d in last newrope : move newrope dirs
    where submove (h:t:rs) d
            | abs a <= 1 && abs b <= 1 = (h+d):t:rs
            | otherwise = (h+d):submove (t:rs) (signum a, signum b)
            where (a,b) = h+d-t
          submove [h] d = [h+d]
move rope [] = [last rope]

move' :: [(Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
move' rope (d:dirs) = let newrope = submove rope d in newrope : move' newrope dirs
    where submove (h:t:rs) d
            | abs a <= 1 && abs b <= 1 = (h+d):t:rs
            | otherwise = (h+d):submove (t:rs) (signum a, signum b)
            where (a,b) = h+d-t
          submove [h] d = [h+d]
move' rope [] = [rope]

instance (Num a, Num b) => Num (a,b) where
    (a,b) + (c,d) = (a+c,b+d)
    negate (a,b) = (-a,-b)  
    (a,b) * (c,d) = (a*c,b*d)