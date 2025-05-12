module Day11 where

import Data.List (transpose, findIndices)

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2023/11.txt"
    let l = lines contents
    let (empty,coords) = (getEmpty l, getCoords l)
    -- part 1
    print $ sum $ taxicabPairs $ transformCoords 1 empty coords
    -- part 2
    print $ sum $ taxicabPairs $ transformCoords 999999 empty coords

getEmpty :: [[Char]] -> ([Int], [Int])
getEmpty ls = (\[a,b] -> (a,b)) . map (findIndices ('#' `notElem`)) $ [id,transpose] <*> pure ls

getCoords :: [[Char]] -> [(Int, Int)]
getCoords lss = map (\(x,y,c) -> (x,y)) $ concatMap (filter (\(x,y,c) -> c == '#')) (zipWith (\y xcs -> map (\(x,c) -> (x,y,c)) xcs) [0..] $ map (zip [0..]) lss)

transformCoords :: Int -> ([Int], [Int]) -> [(Int,Int)] -> [(Int,Int)]
transformCoords n (rows,cols) = map (\(x,y) -> (x + n * length (takeWhile (<x) cols),y + n * length (takeWhile (<y) rows)))

taxicabPairs :: [(Int,Int)] -> [Int]
taxicabPairs (c:coords) = map (taxicab c) coords ++ taxicabPairs coords
taxicabPairs [] = []

taxicab :: Num a => (a, a) -> (a, a) -> a
taxicab (a,b) (c,d) = abs (a-c) + abs (b-d)