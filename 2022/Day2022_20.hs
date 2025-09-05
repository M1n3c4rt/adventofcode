module Day2022_20 where

import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "/home/miney/code/haskell/adventofcode/2022/20.txt"
    -- part 1
    print $ coords $ mix $ parse contents
    -- part 2
    print $ coords $ mix' $ parse contents

type List = S.Set (Int,Int,Int)
--values: (new index) (old index) (value)

search :: ((Int,Int,Int) -> a) -> ((Int,Int,Int) -> Int) -> Int -> List -> a
search f g k = f . S.elemAt 0 . S.filter ((==k) . g)

parse :: String -> List
parse = S.fromList . zip3 [0..] [0..] . map read . lines

move :: Int -> Int -> List -> List
move size n ls =
    let (new,val) = search (\(a,_,c) -> (a,c)) (\(_,b,_) -> b) n ls
        moved_index = let n' = new+val in n' `mod` (size - 1)
        new_ls
            | moved_index < new = S.insert (moved_index,n,val) $ S.delete (new,n,val) $ S.map (\(a,b,c) -> if a >= moved_index && a < new then (a+1,b,c) else (a,b,c)) ls
            | moved_index > new = S.insert (moved_index,n,val) $ S.delete (new,n,val) $ S.map (\(a,b,c) -> if a <= moved_index && a > new then (a-1,b,c) else (a,b,c)) ls
            | otherwise = ls
    in new_ls

mix :: List -> List
mix ls = let size = S.size ls in foldl (flip (move size)) ls [0..size-1]

mix' :: List -> List
mix' ls =
    let size = S.size ls
        ls' = S.map (\(a,b,c) -> (a,b,c*811589153)) ls
    in foldl (flip (move size)) ls' $ concat $ replicate 10 [0..size-1]

coords :: List -> Int
coords ls =
    let size = S.size ls
        zero = search (\(a,_,_) -> a) (\(_,_,c) -> c) 0 ls
        cs = map (\n -> search (\(_,_,c) -> c) (\(a,_,_) -> a) ((zero+n) `mod` size) ls) [1000,2000,3000]
    in sum cs