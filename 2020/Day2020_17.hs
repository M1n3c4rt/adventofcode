module Day2020_17 where

import Utility.AOC (enumerateFilter, neighbours26)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/17.txt"
    let grid = S.fromList $ map (\(x,y) -> (x,y,0)) $ enumerateFilter (=='#') contents
        grid' = S.fromList $ map (\(x,y) -> (x,y,0,0)) $ enumerateFilter (=='#') contents
    -- part 1
    print $ S.size $ (!!6) $ iterate step grid
    -- part 2
    print $ S.size $ (!!6) $ iterate step' grid'

bounds :: S.Set (Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
bounds grid =
    (
        minimum $ S.map (\(x,y,z) -> x) grid,
        maximum $ S.map (\(x,y,z) -> x) grid,
        minimum $ S.map (\(x,y,z) -> y) grid,
        maximum $ S.map (\(x,y,z) -> y) grid,
        minimum $ S.map (\(x,y,z) -> z) grid,
        maximum $ S.map (\(x,y,z) -> z) grid
    )

bounds' :: S.Set (Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int)
bounds' grid =
    (
        minimum $ S.map (\(x,y,z,w) -> x) grid,
        maximum $ S.map (\(x,y,z,w) -> x) grid,
        minimum $ S.map (\(x,y,z,w) -> y) grid,
        maximum $ S.map (\(x,y,z,w) -> y) grid,
        minimum $ S.map (\(x,y,z,w) -> z) grid,
        maximum $ S.map (\(x,y,z,w) -> z) grid,
        minimum $ S.map (\(x,y,z,w) -> w) grid,
        maximum $ S.map (\(x,y,z,w) -> w) grid
    )

step :: S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
step grid = S.fromList $ filter helper [(a,b,c) | a <- [x-1..x'+1], b <- [y-1..y'+1], c <- [z-1..z'+1]]
    where
        (x,x',y,y',z,z') = bounds grid
        helper p
            | p `S.member` grid = length (filter (`S.member` grid) ns) `elem` [2,3]
            | otherwise = length (filter (`S.member` grid) ns) == 3
            where ns = neighbours26 p

step' :: S.Set (Int, Int, Int, Int) -> S.Set (Int, Int, Int, Int)
step' grid = S.fromList $ filter helper [(a,b,c,d) | a <- [x-1..x'+1], b <- [y-1..y'+1], c <- [z-1..z'+1], d <- [w-1..w'+1]]
    where
        (x,x',y,y',z,z',w,w') = bounds' grid
        helper p
            | p `S.member` grid = length (filter (`S.member` grid) ns) `elem` [2,3]
            | otherwise = length (filter (`S.member` grid) ns) == 3
            where
                ns = neighbours80 p
                neighbours80 (a,b,c,d) = [(a+p,b+q,c+r,d+s) | p <- [-1,0,1], q <- [-1,0,1], r <- [-1,0,1], s <- [-1,0,1], any (/=0) [p,q,r,s]]