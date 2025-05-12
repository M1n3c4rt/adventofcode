module Day24 where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2020/24.txt"
    let blacktiles = S.fromList $ HM.keys $ HM.filter odd $ accumulate $ map (identify . parse) $ lines contents
    -- part 1
    print $ S.size blacktiles
    -- part 2
    print $ S.size $ (!!100) $ iterate step blacktiles

parse :: String -> [(Int,Int)]
parse (a:b:line) = case [a,b] of
    ['w',_] -> (-1,0):parse (b:line)
    ['e',_] -> (1,0):parse (b:line)
    "nw" -> (-1,1):parse line
    "ne" -> (0,1):parse line
    "sw" -> (0,-1):parse line
    "se" -> (1,-1):parse line
parse [a] = case a of
    'w' -> [(-1,0)]
    'e' -> [(1,0)]
parse [] = []

identify :: [(Int, Int)] -> (Int, Int)
identify = foldl (\(x,y) (a,b) -> (x+a,y+b)) (0,0) 

accumulate :: [(Int,Int)] -> HM.HashMap (Int,Int) Int
accumulate = HM.map length . foldr (\c acc -> HM.insertWith (++) c [c] acc) HM.empty

step :: S.Set (Int, Int) -> S.Set (Int, Int)
step tiles = S.fromList $ filter helper [(x,y) | x <- [x1-1..x2+1], y <- [y1-1..y2+1]]
    where
        ((x1,y1),(x2,y2)) = bounds tiles
        helper x@(p,q) = if x `S.member` tiles then neighbours `notElem` [0,3,4,5,6] else neighbours == 2
            where neighbours = length $ filter (`S.member` tiles) $ map (\(a,b) -> (a+p,b+q)) $ parse "wenwneswse" 

bounds :: S.Set (Int, Int) -> ((Int, Int), (Int, Int))
bounds tiles =
    let xs = S.map fst tiles
        ys = S.map snd tiles
    in ((minimum xs,minimum ys),(maximum xs, maximum ys))