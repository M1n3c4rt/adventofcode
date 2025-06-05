module Day2022_18 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/18.txt"
    let cubes = parse contents
    -- part 1
    print $ surfaceArea cubes
    -- part 2
    print $ surfaceArea' (floodFill S.empty (S.singleton (0,0,0)) cubes) cubes

parse :: String -> S.Set (Int,Int,Int)
parse = S.fromList . map ((\[a,b,c] -> (a,b,c)) . map read . splitOn ",") . lines

surfaceArea :: S.Set (Int,Int,Int) -> Int
surfaceArea cubes = sum $ map ((6-) . length . S.filter (`S.member` cubes) . neighbours) $ S.toList cubes

surfaceArea' :: S.Set (Int,Int,Int) -> S.Set (Int,Int,Int) -> Int
surfaceArea' exterior cubes = sum $ map (length . S.filter (`S.member` exterior) . neighbours) $ S.toList cubes

neighbours :: (Int,Int,Int) -> S.Set (Int,Int,Int)
neighbours (x,y,z) = S.fromList [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

floodFill :: S.Set (Int, Int, Int) -> S.Set (Int, Int, Int) -> S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
floodFill finished frontier cubes =
    let newfrontier = S.filter (\p -> inBounds p && p `S.notMember` frontier && p `S.notMember` finished && p `S.notMember` cubes) $ S.unions $ S.map neighbours frontier
    in if S.null frontier then finished else floodFill (S.union finished frontier) newfrontier cubes

inBounds :: (Int, Int, Int) -> Bool
inBounds (x,y,z) = all (\p -> p >= -1 && p <= 20) [x,y,z]